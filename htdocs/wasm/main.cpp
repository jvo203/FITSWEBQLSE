#include <emscripten/bind.h>
#include <emscripten.h>

using namespace emscripten;

val decompressZFPimage(int img_width, int img_height, std::string const &bytes)
{
  size_t img_size = size_t(img_width) * size_t(img_height);
  float *vec = (float *)calloc(img_size, sizeof(float));

  for (size_t i = 0; i < img_size; i++)
    vec[i] = 0.5f;

  return val(typed_memory_view(img_size, vec));
}

val decompressLZ4mask(int img_width, int img_height, std::string const &bytes)
{
  size_t img_size = size_t(img_width) * size_t(img_height);
  unsigned char *vec = (unsigned char *)calloc(img_size, sizeof(unsigned char));

  memset(vec, 255, img_size); // fill-in the mask

  return val(typed_memory_view(img_size, vec));
}

EMSCRIPTEN_BINDINGS(Test)
{
  function("decompressZFPimage", &decompressZFPimage);
  function("decompressLZ4mask", &decompressLZ4mask);
}

int main()
{
  EM_ASM(
      function test_array(arr, res) {
        console.log("array", arr, "length", res.length, "res[first]", res[0], "res[last]", res[res.length - 1]);
      };

      const img_width = 2502;
      const img_height = 2502;

      /*const img_width = 500;
      const img_height = 500;*/

      const pixels = Module.decompressZFPimage(img_width, img_height, "frame_pixels");
      const mask = Module.decompressLZ4mask(img_width, img_height, "frame_mask");

      test_array("pixels", pixels);
      test_array("mask", mask);

      const timeout = 5000;
      setTimeout(
          function() {
            test_array("pixels", pixels);
            test_array("mask", mask);
          },
          timeout);

      console.log(`waiting ${timeout} ms...`));
}
