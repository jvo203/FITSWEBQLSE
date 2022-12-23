#include <cuda_runtime.h>
#include <helper_cuda.h>
#include <nppi.h>

#include <math.h>

#include <fstream>  // ifstream
#include <iostream> // cout, cerr
#include <sstream>  // stringstream
using namespace std;

inline int findCudaDevice() {
  cudaDeviceProp deviceProp;
  int devID = 0;

  // Otherwise pick the device with highest Gflops/s
  devID = gpuGetMaxGflopsDeviceId();
  checkCudaErrors(cudaSetDevice(devID));
  checkCudaErrors(cudaGetDeviceProperties(&deviceProp, devID));
  printf("GPU Device %d: \"%s\" with compute capability %d.%d\n\n", devID,
         deviceProp.name, deviceProp.major, deviceProp.minor);

  return devID;
}

inline int cudaDeviceInit() {
  int deviceCount;
  checkCudaErrors(cudaGetDeviceCount(&deviceCount));

  if (deviceCount == 0) {
    std::cerr << "CUDA error: no devices supporting CUDA." << std::endl;
    exit(EXIT_FAILURE);
  }

  int dev = findCudaDevice();

  cudaDeviceProp deviceProp;
  cudaGetDeviceProperties(&deviceProp, dev);
  std::cerr << "cudaSetDevice GPU" << dev << " = " << deviceProp.name
            << std::endl;

  checkCudaErrors(cudaSetDevice(dev));

  return dev;
}

int main() {
  // initalize cuda device
  int devID = cudaDeviceInit();
  if (devID != 0)
    throw std::runtime_error("cudaDeviceInit fail ");

  int x = 0, y = 0, width = 0, height = 0, maxval = 0;

  string inputLine = "";
  std::string filename = "one.pgm";
  std::ifstream pgm_file(filename, std::ios::out | std::ios::binary);

  getline(pgm_file, inputLine);

  if (inputLine.compare("P5") != 0)
    cerr << "Version error" << endl;
  else
    cout << "Version : " << inputLine << endl;

  // Second line : comment
  getline(pgm_file, inputLine);
  cout << "Comment : " << inputLine << endl;

  pgm_file >> width >> height >> maxval;
  cout << width << " x " << height << " maxval: " << maxval << endl;

  size_t img_size = width * height;
  uint8_t array[img_size];

  pgm_file.read((char *)array, img_size);
  pgm_file.close();

  {
    // export luma to a PGM file for a cross-check
    std::string filename = "one_src.pgm";
    std::fstream pgm_file(filename, std::ios::out | std::ios::binary);

    pgm_file << "P5" << std::endl;
    pgm_file << width << " " << height << " 255" << std::endl;
    pgm_file.write((const char *)array, img_size);
    pgm_file.close();
  }

  // prepare the source arrays
  float *pix32f = (float *)calloc(img_size, sizeof(float));
  uint8_t *pix8u = (uint8_t *)calloc(img_size, sizeof(uint8_t));

  for (size_t i = 0; i < img_size; i++) {
    pix8u[i] = array[i];
    pix32f[i] = (float)array[i];
  }

  // the resize part
  int img_width = width / 2;
  int img_height = height / 2;
  size_t plane_size = img_width * img_height;

  std::cout << "after downsizing: " << img_width << " x " << img_height
            << std::endl;

  float *dstPix32f = (float *)calloc(plane_size, sizeof(float));
  uint8_t *dstPix8u = (uint8_t *)calloc(plane_size, sizeof(uint8_t));

  // 8-bit unsigned integer pixels
  {
    cudaError_t cudaRet;
    int nSrcStep, nDstStep;

    // need to alloc cuda memory for source
    Npp8u *pSrc = nppiMalloc_8u_C1(width, height, &nSrcStep);

    printf("nSrcStep %d \n", nSrcStep);

    // Need to copy image from Host to GPU Pay attention GPU memory is in power
    // of 2 thus stride copy is required
    cudaRet = cudaMemcpy2D(pSrc, nSrcStep, pix8u, width, width, height,
                           cudaMemcpyHostToDevice);

    if (cudaRet != cudaSuccess)
      throw std::runtime_error("cudaMemcpyHostToDevice fail ");

    // need to alloc cuda memory for destination
    Npp8u *pDst = nppiMalloc_8u_C1(img_width, img_height, &nDstStep);

    printf("nDstStep %d \n", nDstStep);

    NppiSize srcSize;
    srcSize.width = width;
    srcSize.height = height;
    NppiRect srcROI = {0, 0, srcSize.width, srcSize.height};

    NppiSize dstSize;
    dstSize.width = img_width;
    dstSize.height = img_height;

    NppiRect dstROI = {0, 0, dstSize.width, dstSize.height};

    NppStatus status = nppiResize_8u_C1R(pSrc, nSrcStep, srcSize, srcROI, pDst,
                                         nDstStep, dstSize, dstROI,
                                         // NPPI_INTER_LINEAR
                                         NPPI_INTER_LANCZOS
                                         // NPPI_INTER_LANCZOS3_ADVANCED
    );

    std::cout << "Npp8u::NppStatus = " << status << std::endl;

    if (status == NPP_SUCCESS) {
      // Need to copy image from GPU to HOST Pay attention GPU memory is in
      // power of 2 thus stride copy is required
      cudaRet = cudaMemcpy2D(dstPix8u, img_width, pDst, nDstStep, img_width,
                             img_height, cudaMemcpyDeviceToHost);

      if (cudaRet != cudaSuccess)
        throw std::runtime_error("cudaMemcpyDeviceToHost fail ");

      // export luma to a PGM file for a cross-check
      std::string filename = "one_half_nppi.pgm";
      std::fstream pgm_file(filename, std::ios::out | std::ios::binary);

      pgm_file << "P5" << std::endl;
      pgm_file << img_width << " " << img_height << " 255" << std::endl;
      pgm_file.write((const char *)dstPix8u, plane_size);
      pgm_file.close();
    }

    nppiFree(pSrc);
    nppiFree(pDst);
  }

  // 32-bit floating-point pixels
  {
    cudaError_t cudaRet;
    int nSrcStep, nDstStep;

    // need to alloc cuda memory for source
    Npp32f *pSrc = nppiMalloc_32f_C1(width, height, &nSrcStep);

    printf("nSrcStep %d \n", nSrcStep);

    // Need to copy image from Host to GPU Pay attention GPU memory is in power
    // of 2 thus stride copy is required
    cudaRet =
        cudaMemcpy2D(pSrc, nSrcStep, pix32f, width * sizeof(float),
                     width * sizeof(float), height, cudaMemcpyHostToDevice);

    if (cudaRet != cudaSuccess)
      throw std::runtime_error("cudaMemcpyHostToDevice fail ");

    // need to alloc cuda memory for destination
    Npp32f *pDst = nppiMalloc_32f_C1(img_width, img_height, &nDstStep);
    Npp32f *pMirror = nppiMalloc_32f_C1(img_width, img_height, &nDstStep);

    printf("nDstStep %d \n", nDstStep);

    NppiSize srcSize;
    srcSize.width = width;
    srcSize.height = height;
    NppiRect srcROI = {0, 0, srcSize.width, srcSize.height};

    NppiSize dstSize;
    dstSize.width = img_width;
    dstSize.height = img_height;
    NppiRect dstROI = {0, 0, dstSize.width, dstSize.height};

    NppStatus status = nppiResize_32f_C1R(pSrc, nSrcStep, srcSize, srcROI, pDst,
                                          nDstStep, dstSize, dstROI,
                                          // NPPI_INTER_LINEAR
                                          NPPI_INTER_LANCZOS
                                          // NPPI_INTER_LANCZOS3_ADVANCED
    );

    std::cout << "nppiResize_32f_C1R::NppStatus = " << status << std::endl;

    NppiSize oROI = {dstSize.width, dstSize.height};

    // cannot be done in place, an extra buffer to hold the inverted image must
    // be used
    NppStatus mirrorStatus = nppiMirror_32f_C1R(
        pDst, nDstStep, pMirror, nDstStep, oROI, NPP_HORIZONTAL_AXIS);

    std::cout << "nppiMirror_32f_C1R::NppStatus = " << mirrorStatus
              << std::endl;

    if (status == NPP_SUCCESS && mirrorStatus == NPP_SUCCESS) {
      // Need to copy image from GPU to HOST Pay attention GPU memory is in
      // power of 2 thus stride copy is required
      cudaRet = cudaMemcpy2D(dstPix32f, img_width * sizeof(float), pMirror,
                             nDstStep, img_width * sizeof(float), img_height,
                             cudaMemcpyDeviceToHost);

      if (cudaRet != cudaSuccess)
        throw std::runtime_error("cudaMemcpyDeviceToHost fail ");

      for (size_t i = 0; i < plane_size; i++)
        dstPix8u[i] = roundf(dstPix32f[i]);

      // export luma to a PGM file for a cross-check
      std::string filename = "one_half_float_nppi.pgm";
      std::fstream pgm_file(filename, std::ios::out | std::ios::binary);

      pgm_file << "P5" << std::endl;
      pgm_file << img_width << " " << img_height << " 255" << std::endl;
      pgm_file.write((const char *)dstPix8u, plane_size);
      pgm_file.close();
    }

    nppiFree(pSrc);
    nppiFree(pDst);
    nppiFree(pMirror);
  }

  // release the memory
  free(pix32f);
  free(pix8u);

  free(dstPix32f);
  free(dstPix8u);
}