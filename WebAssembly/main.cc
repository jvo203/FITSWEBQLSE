#include <emscripten.h>
#include <emscripten/bind.h>
#include <emscripten/val.h>

extern "C"
{
// HEVC video decoder
#include "hevc_decoder.h"
}

// OpenEXR image decoder
#include <OpenEXR/IlmThread.h>
#include <OpenEXR/ImfNamespace.h>
#include <OpenEXR/ImfThreading.h>

#include <OpenEXR/ImfChannelList.h>
#include <OpenEXR/ImfHeader.h>
#include <OpenEXR/ImfIO.h>
#include <OpenEXR/ImfInputFile.h>

#include <algorithm>
#include <cstdint>
#include <map>
#include <stdexcept>
#include <string>
#include <vector>

#include <fpzip/include/fpzip.h>

using namespace emscripten;

class BufferAdapter : public OPENEXR_IMF_NAMESPACE::IStream
{
public:
  BufferAdapter(char *buffer, size_t size)
      : IStream("BufferAdapter"), m_buffer(buffer), m_size(size), m_offset() {}

  virtual bool isMemoryMapped() const override { return true; }

  virtual bool read(char c[/*n*/], int n) override
  {
    std::size_t remaining = m_size - m_offset;
    if (std::size_t(n) > remaining)
      throw std::runtime_error("Out of bounds read request");
    auto current = readMemoryMapped(n);
    std::copy(current, current + n, c);
    return n == remaining;
  }

  virtual char *readMemoryMapped(int n) override
  {
    std::size_t remaining = m_size - m_offset;
    if (std::size_t(n) > remaining)
      throw std::runtime_error("Out of bounds read request");
    auto current = bufferCurrent();
    m_offset += n;
    return current;
  }

  virtual OPENEXR_IMF_NAMESPACE::Int64 tellg() override { return m_offset; }

  virtual void seekg(OPENEXR_IMF_NAMESPACE::Int64 pos) override
  {
    m_offset = pos;
  }

public:
  char *bufferCurrent() const { return m_buffer + m_offset; }

private:
  char *m_buffer;
  std::size_t m_size;
  std::size_t m_offset;
};

typedef std::vector<char> Bytes;
typedef std::vector<float> Pixel;
typedef std::map<std::string, Pixel> Planes;

struct EXRImage
{
  std::size_t width;
  std::size_t height;
  Planes planes;

  emscripten::val plane(std::string const &name) const
  {
    using namespace emscripten;
    auto it = planes.find(name);
    if (it == planes.end())
      return val::undefined();
    return val(typed_memory_view(it->second.size(), it->second.data()));
  }

  emscripten::val channels() const
  {
    using namespace emscripten;
    auto c = val::array();
    std::size_t idx = 0;
    for (auto const &plane : planes)
      c.set(idx++, plane.first);
    return val(c);
  }
};

EXRImage loadEXRRaw(char const *buffer, std::size_t size)
{
  using namespace OPENEXR_IMF_NAMESPACE;

  EXRImage image;
  try
  {
    BufferAdapter bytes(const_cast<char *>(buffer), size);
    InputFile file(bytes);
    auto window = file.header().dataWindow();
    image.width = window.max.x - window.min.x + 1;
    image.height = window.max.y - window.min.y + 1;
    size_t strideX = sizeof(float);
    size_t strideY = sizeof(float) * image.width;
    FrameBuffer fb;
    auto const &channels = file.header().channels();
    auto it = channels.begin();
    auto ed = channels.end();
    for (; it != ed; ++it) // OpenEXR iterators are not real iterators so ranged
                           // for loop doesn't work
    {
      auto const &name = it.name();
      image.planes[name].resize(image.width * image.height);
      // OpenEXR needs to be passed a pointer that, when the window is applied,
      // leads to the start of the data buffer, even if that pointer is illegal
      auto startOffset = window.min.x + window.min.y * image.width;
      fb.insert(name, Slice(FLOAT,
                            reinterpret_cast<char *>(image.planes[name].data() -
                                                     startOffset),
                            strideX, strideY));
    }
    file.setFrameBuffer(fb);
    file.readPixels(window.min.y, window.max.y);
  }
  catch (...)
  {
  }
  return image;
}

EXRImage loadEXRVec(std::vector<char> const &bytes)
{
  return loadEXRRaw(bytes.data(), bytes.size());
}

EXRImage loadEXRStr(std::string const &bytes)
{
  return loadEXRRaw(bytes.data(), bytes.size());
}

void enableMultithreading(int no_threads)
{
  if (ILMTHREAD_NAMESPACE::supportsThreads())
  {
    OPENEXR_IMF_NAMESPACE::setGlobalThreadCount(no_threads);
    std::cout << "[OpenEXR] number of threads: "
              << OPENEXR_IMF_NAMESPACE::globalThreadCount() << std::endl;
  }
}

std::vector<float> FPunzip(std::string const &bytes)
{
  std::cout << "[fpunzip] " << bytes.size() << " bytes." << std::endl;

  FPZ *fpz = fpzip_read_from_buffer(bytes.data());

  /* read header */
  if (!fpzip_read_header(fpz))
  {
    fprintf(stderr, "cannot read header: %s\n", fpzip_errstr[fpzip_errno]);
    return std::vector<float>();
  }

  // decompress into <spectrum.data()>
  uint32_t spec_len = fpz->nx;

  if (spec_len == 0)
  {
    fprintf(stderr, "zero-sized fpzip array\n");
    return std::vector<float>();
  }

  std::vector<float> spectrum(spec_len, 0.0f);

  if ((fpz->ny != 1) || (fpz->nz != 1) || (fpz->nf != 1))
  {
    fprintf(stderr, "array size does not match dimensions from header\n");
    return std::vector<float>();
  }

  /* perform actual decompression */
  if (!fpzip_read(fpz, spectrum.data()))
  {
    fprintf(stderr, "decompression failed: %s\n", fpzip_errstr[fpzip_errno]);
    return std::vector<float>();
  }

  fpzip_read_close(fpz);

  return spectrum;
}

void hevc_init_frame(int va_count, int width, int height)
{
  size_t len = width * height * 4;

  if (canvasLength != len)
  {
    if (canvasBuffer != NULL)
      free(canvasBuffer);

    canvasBuffer = NULL;
    canvasLength = 0;
  }

  if (canvasBuffer == NULL)
  {
    canvasBuffer = (unsigned char *)malloc(len);

    if (canvasBuffer != NULL)
      canvasLength = len;

    printf("[hevc_init_frame] width: %d, height: %d, canvasLength = %zu, canvasBuffer = %p\n", width, height, canvasLength, canvasBuffer);
  }

  hevc_init(va_count);

  printf("[hevc_init_frame] done.\n");
}

void hevc_destroy_frame(int va_count)
{
  if (canvasBuffer != NULL)
  {
    free(canvasBuffer);

    canvasBuffer = NULL;
    canvasLength = 0;
  }

  hevc_destroy(va_count);

  printf("[hevc_destroy_frame] done.\n");
}

val hevc_decode_frame(unsigned int _w, unsigned int _h, std::string const &bytes, int index, std::string const &colourmap, unsigned char fill)
{  
  size_t len = _w * _h * 4;

  if (canvasBuffer != NULL && canvasLength == len)
    hevc_decode_nal_unit(index, (unsigned char *)bytes.data(), bytes.size(), canvasBuffer, _w, _h, colourmap.c_str(), fill);
  else
  {
    printf("canvasBuffer(%p) == NULL and/or canvasLength(%zu) does not match len(%zu)\n", canvasBuffer, canvasLength, len);

    hevc_decode_nal_unit(index, (unsigned char *)bytes.data(), bytes.size(), NULL, _w, _h, colourmap.c_str(), fill);
  }

  return val(typed_memory_view(canvasLength, canvasBuffer));
}

EMSCRIPTEN_BINDINGS(Wrapper)
{
  register_vector<float>("Pixel");
  register_vector<char>("Bytes");
  register_map<std::string, Pixel>("Planes");
  class_<EXRImage>("EXRImage")
      .property("width", &EXRImage::width)
      .property("height", &EXRImage::height)
      .property("planes", &EXRImage::planes)
      .function("plane", &EXRImage::plane)
      .function("channels", &EXRImage::channels);
  function("loadEXRRaw", &loadEXRRaw, allow_raw_pointer<arg<0>>());
  function("loadEXRVec", &loadEXRVec);
  function("loadEXRStr", &loadEXRStr);
  function("enableMultithreading", &enableMultithreading);
  function("FPunzip", &FPunzip);
  function("hevc_init_frame", &hevc_init_frame);
  function("hevc_destroy_frame", &hevc_destroy_frame);
  function("hevc_decode_frame", &hevc_decode_frame);
}