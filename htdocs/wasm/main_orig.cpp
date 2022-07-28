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

EMSCRIPTEN_BINDINGS(Test) { 
  function("decompressZFPimage", &decompressZFPimage);
}

int main() {
  EM_ASM(
    const img_width = 2502;
    const img_height = 2502;
    const res = Module.decompressZFPimage(img_width, img_height, "frame_pixels");
    console.log(`length: ${res.length} res[0]: ${res[0]} res[last]: ${res[res.length - 1]}`);
    const timeout = 5000;
    setTimeout(() => {
      console.log(`length: ${res.length} res[0]: ${res[0]} res[last]: ${res[res.length - 1]}`);
    }, timeout);
    console.log(`waiting ${timeout}ms...`)
  );
}
