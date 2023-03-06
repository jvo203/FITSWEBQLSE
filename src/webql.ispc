typedef int16 int16_t;
typedef int32 int32_t;
typedef int64 int64_t;
typedef unsigned int16 uint16_t;
typedef unsigned int32 uint32_t;
typedef unsigned int64 uint64_t;

#define FLT_MIN 1.175494351e-38F
#define FLT_MAX 3.402823466e+38F

/* FIXED-POINT COMPRESSION */
#define BASE 4
#define DIM 16
#define BLOCK_SIZE (DIM * DIM) /* values per block */
#define significant_bits 7

struct fixed_block_t {
    int16 mask[DIM];
    int16 common_exp;
    int8 mantissa[BLOCK_SIZE];
};
/* END OF FIXED POINT */

extern "C" uniform float erff(uniform float);

uint16_t bswap16(uint16_t u)
{
  return ((u&0xFF)<<8) | (((u>>8)&0xFF)<<0);
}

uint32_t bswap32(uint32_t u)
{
  return ((u&0xFF)<<24) | (((u>>8)&0xFF)<<16) | (((u>>16)&0xFF)<<8) | (((u>>24)&0xFF)<<0);
}

uint64_t bswap64(uint64_t u)
{
  return ((u&0xFF)<<56) | (((u>>8)&0xFF)<<48) | (((u>>16)&0xFF)<<40) | (((u>>24)&0xFF)<<32) | (((u>>32)&0xFF)<<24) | (((u>>40)&0xFF)<<16) | (((u>>48)&0xFF)<<8) | (((u>>56)&0xFF)<<0);
}

bool isinf(float tmp)
{
  unsigned int i = intbits(tmp) ;
  return ( (i & 0x7fffffff) == 0x7f800000 ) || ( (i & 0xffffffff) == 0xff800000 ) ;
}

export void  make_image_spectrumF32(uniform float src[], uniform float pixels[], uniform unsigned int8 mask[], uniform unsigned int8 data_mask[], uniform double ignrval, uniform double datamin, uniform double datamax,  uniform double cdelt3, uniform float res[], uniform int64 npixels)
{
  uniform float frame_min, frame_max;
  uniform double mean, integrated;

  frame_min = res[0];
  frame_max = res[1];

  varying double partial_sum = 0.0 ;
  varying unsigned int64 partial_count = 0 ;

  varying float vmin = frame_min ;
  varying float vmax = frame_max ;

  // print ("npixels: %, frame_min: %, frame_max: %, datamin: %, datamax: %, cdelt3: %, ignrval: %\n", npixels, frame_min, frame_max, datamin, datamax, cdelt3, ignrval);

  foreach(i=0 ... npixels)
    {      
      float tmp = src[i];

      //next the min/max values & the mask
      cif(!(isnan(tmp) || isinf(tmp)) && (tmp > ignrval) && (tmp >= datamin) && (tmp <= datamax))
        {
            vmin = min(vmin, tmp) ;
            vmax = max(vmax, tmp) ;

            pixels[i] += tmp * cdelt3;
            mask[i] |= true;
            data_mask[i] = true;

            partial_sum += tmp ;
            partial_count += 1 ;
        } 
        else
          data_mask[i] = false;
    }

uniform double sum = reduce_add(partial_sum) ;
uniform int64 count = reduce_add(partial_count) ;

  if(count > 0)
    {

      mean = sum / (double) count ;
      integrated = sum * cdelt3 ;
    }
  else
    {      
      mean = 0.0 ;
      integrated = 0.0 ;
    }
  
  // print ("npixels: %, cdelt3: %, mean: %, integrated: %, sum: %, count: %\n", npixels, cdelt3, mean, integrated, sum, count);

  frame_min = reduce_min(vmin) ;
  frame_max = reduce_max(vmax) ;

  res[0] = frame_min;
  res[1] = frame_max;
  res[2] = (float) mean;
  res[3] = (float) integrated;
} ;

export void array_stat(uniform float pixels[], uniform float pmin[], uniform float pmax[], uniform float pmean[], uniform int64 npixels)
{  
  varying float vmin = pmin[0] ;
  varying float vmax = pmax[0] ;
  varying double vsum = 0.0 ;

  foreach(i=0 ... npixels)
    {      
      float tmp = pixels[i];

      vmin = min(vmin, tmp) ;
      vmax = max(vmax, tmp) ;
      vsum += tmp ;
    }

  uniform double psum = reduce_add(vsum) ;

  pmin[0] = reduce_min(vmin) ;
  pmax[0] = reduce_max(vmax) ;

  if(npixels > 0)
    pmean[0] = (float) (psum / (uniform double) npixels) ;
  else
    pmean[0] = 0.0 ;
}

export uniform float array_std(uniform float pixels[], uniform float pmean, uniform int64 npixels)
{
  varying double vsum = 0.0 ;

  foreach(i=0 ... npixels)
    {      
      float tmp = pixels[i];
      vsum += (tmp - pmean) * (tmp - pmean) ;
    }

  uniform double psum = reduce_add(vsum) ;

  if(npixels > 2)
    return (float) sqrt(psum / (uniform double) (npixels - 1)) ;
  else    
    return (float) sqrt(psum / (uniform double) npixels) ;
}

export void array_erf(uniform float pixels[], uniform float pmean, uniform float pstd, uniform int64 npixels)
{
  foreach(i=0 ... npixels)
    {      
      /*varying float x = (pixels[i] - pmean) / (pstd * sqrt(2.0f)); // a proper CFD      
      
      foreach_active(instance)
      {      
        uniform float r = erff(extract(x, instance));
        x = insert(x, instance, r);
      }

      pixels[i] = 0.5f * (1.0f + x);*/

      pixels[i] = (pixels[i] - pmean) / pstd; // non-scaled ERF      
    }
}

export uniform float brightness_ratio(uniform float pixels[], uniform float black, uniform float sensitivity, uniform int offset, uniform int total_size)
{
  float partial_sum = 0.0f ;  

  foreach(j=0 ... total_size)
  {    
    float pixel = 5.0f * (pixels[offset + j] - black) * sensitivity ;
    partial_sum += pixel > 0.0f ? pixel/(1.0f + pixel) : 0.0f ;    
  }

  uniform float sum = reduce_add(partial_sum);
  // print("black: %, sensitivity: %, sum: %\n", black, sensitivity, sum);

  return sum;
} ;

export uniform float calculate_radial_spectrumF16(uniform int16 cubeData[], uniform unsigned int stride, uniform int x1, uniform int x2, uniform int y1, uniform int y2, uniform int cx, uniform int cy, uniform int r2, uniform bool average, uniform float cdelt3)
{  
  double partial_sum = 0.0 ;
  unsigned int64 partial_count = 0 ;
  
  foreach (y=y1 ... y2, x=x1 ... x2)
    {      
      float tmp = half_to_float_fast(cubeData[y*stride+x]) ;
      bool nan = isnan(tmp) ;

      float dist2 = (cx-x)*(cx-x) + (cy-y)*(cy-y) ;
      bool exclude = (dist2 > r2) ;
      
      partial_sum += (nan || exclude) ? 0.0f : tmp ;
      partial_count += (nan || exclude) ? 0 : 1 ;
    } ;

  uniform double sum = reduce_add(partial_sum) ;
  uniform int64 count = reduce_add(partial_count) ;

  if(count > 0)
    {
      if(average)
	      return (float)(sum / (double) count) ;
      else
	      return (float)(sum * cdelt3) ;
    }
  else
    return 0.0f ;
} ;

export uniform float calculate_square_spectrumF16(uniform int16 cubeData[], uniform unsigned int stride, uniform int x1, uniform int x2, uniform int y1, uniform int y2, uniform bool average, uniform float cdelt3)
{
  double partial_sum = 0.0 ;
  unsigned int64 partial_count = 0 ;
  
  foreach (y=y1 ... y2, x=x1 ... x2)
    {      
      float tmp = half_to_float_fast(cubeData[y*stride+x]) ;
      bool nan = isnan(tmp) ;
      
      partial_sum += nan ? 0.0f : tmp ;
      partial_count += nan ? 0 : 1 ;
    } ;

  uniform double sum = reduce_add(partial_sum) ;
  uniform int64 count = reduce_add(partial_count) ;

  if(count > 0)
    {
      if(average)
	      return (float)(sum / (double) count) ;
      else
	      return (float)(sum * cdelt3) ;
    }
  else
    return 0.0f ;
} ;

export uniform float calculate_radial_viewport_spectrumF16(uniform int16 cubeData[], uniform unsigned int cubeStride, uniform float pixels[], uniform bool mask[], uniform unsigned int stride, uniform int x1, uniform int x2, uniform int y1, uniform int y2, uniform int cx, uniform int cy, uniform int r2, uniform bool average, uniform float cdelt3)
{  
  double partial_sum = 0.0 ;
  unsigned int64 partial_count = 0 ;
  
  foreach (y=y1 ... y2, x=x1 ... x2)
    {      
      float tmp = half_to_float_fast(cubeData[y*cubeStride+x]) ;
      bool nan = isnan(tmp) ;

      float dist2 = (cx-x)*(cx-x) + (cy-y)*(cy-y) ;
      bool exclude = (dist2 > r2) ;
      
      partial_sum += (nan || exclude) ? 0.0f : tmp ;
      partial_count += (nan || exclude) ? 0 : 1 ;

      // the still viewport
      varying int idx = (y-y1)*stride+(x-x1) ;
      pixels[idx] += (nan || exclude) ? 0.0f : tmp ;
      mask[idx] |= (nan || exclude) ? false : true ;
    } ;

  uniform double sum = reduce_add(partial_sum) ;
  uniform int64 count = reduce_add(partial_count) ;

  if(count > 0)
    {
      if(average)
	      return (float)(sum / (double) count) ;
      else
	      return (float)(sum * cdelt3) ;
    }
  else
    return 0.0f ;
} ;

export uniform float calculate_square_viewport_spectrumF16(uniform int16 cubeData[], uniform unsigned int cubeStride, uniform float pixels[], uniform bool mask[], uniform unsigned int stride, uniform int x1, uniform int x2, uniform int y1, uniform int y2, uniform bool average, uniform float cdelt3)
{
  double partial_sum = 0.0 ;
  unsigned int64 partial_count = 0 ;
  
  foreach (y=y1 ... y2, x=x1 ... x2)
    {      
      float tmp = half_to_float_fast(cubeData[y*cubeStride+x]) ;
      bool nan = isnan(tmp) ;
      
      partial_sum += nan ? 0.0f : tmp ;
      partial_count += nan ? 0 : 1 ;

      // the still viewport
      int idx = (y-y1)*stride+(x-x1) ;
      pixels[idx] += nan ? 0.0f : tmp ;
      mask[idx] |= nan ? false : true ;
    } ;

  uniform double sum = reduce_add(partial_sum) ;
  uniform int64 count = reduce_add(partial_count) ;

  if(count > 0)
    {
      if(average)
	      return (float)(sum / (double) count) ;
      else
	      return (float)(sum * cdelt3) ;
    }
  else
    return 0.0f ;
} ;

/* without any downsizing
foreach (y=0 ... height, x=0 ... width)
(...)
float tmp = half_to_float_fast(src[y*src_stride+x]) ;
unsigned int idx = y * dst_stride + x ;
*/

export void make_video_frameF16_linear(uniform unsigned int16 src[], uniform int width, uniform int height, uniform int src_stride, uniform unsigned int8 dst_luma[], uniform unsigned int8 dst_mask[], uniform int dst_width, uniform int dst_height, uniform int dst_stride, uniform float black, uniform float slope)
{
  uniform float scale_x = (float)width / (float)dst_width ;
  uniform float scale_y = (float)height / (float)dst_height ;

  foreach (y=0 ... dst_height, x=0 ... dst_width)
    {
      // source
      varying int sx = round(scale_x * (float)x) ;
      varying int sy = round(scale_y * (float)y) ;
      
      unsigned int src_x = clamp(sx, (varying int32)0, (varying int32)width-1);
      unsigned int src_y = clamp(sy, (varying int32)0, (varying int32)height-1);
      
      unsigned int src_idx = src_y*src_stride+src_x ;
      float tmp = half_to_float_fast(src[src_idx]) ;

      // destination
      unsigned int idx = y * dst_stride + x ;

      //alternative coherent if
      cif (!isnan(tmp))
	    {        
        float pixel = 255.0f / ( 1.0f + exp(-6.0f * (tmp - black) * slope) ) ;             
        dst_luma[idx] = (int) clamp(pixel, 0.0f, 255.0f) ;
        dst_mask[idx] = 255 ;
	    }
      else
      {
        dst_luma[idx] = 0 ;
	      dst_mask[idx] = 0 ;
      }
    } ;  
} ;

export void make_video_frameF16_logistic(uniform unsigned int16 src[], uniform int width, uniform int height, uniform int src_stride, uniform unsigned int8 dst_luma[], uniform unsigned int8 dst_mask[], uniform int dst_width, uniform int dst_height, uniform int dst_stride, uniform float median, uniform float sensitivity)
{
  uniform float scale_x = (float)width / (float)dst_width ;
  uniform float scale_y = (float)height / (float)dst_height ;

  foreach (y=0 ... dst_height, x=0 ... dst_width)
    {
      // source
      varying int sx = round(scale_x * (float)x) ;
      varying int sy = round(scale_y * (float)y) ;
      
      unsigned int src_x = clamp(sx, (varying int32)0, (varying int32)width-1);
      unsigned int src_y = clamp(sy, (varying int32)0, (varying int32)height-1);
      
      unsigned int src_idx = src_y*src_stride+src_x ;
      float tmp = half_to_float_fast(src[src_idx]) ;

      // destination
      unsigned int idx = y * dst_stride + x ;

      //alternative coherent if
      cif (!isnan(tmp))
	    {
        float pixel = 255.0f / ( 1.0f + exp(-6.0f * (tmp - median) * sensitivity) ) ;          
        dst_luma[idx] = (int) clamp(pixel, 0.0f, 255.0f) ;
        dst_mask[idx] = 255 ;
	    }
      else
      {
        dst_luma[idx] = 0 ;
	      dst_mask[idx] = 0 ;
      }
    } ;  
} ;

export void make_video_frameF16_ratio(uniform unsigned int16 src[], uniform int width, uniform int height, uniform int src_stride, uniform unsigned int8 dst_luma[], uniform unsigned int8 dst_mask[], uniform int dst_width, uniform int dst_height, uniform int dst_stride, uniform float black, uniform float sensitivity)
{
  uniform float scale_x = (float)width / (float)dst_width ;
  uniform float scale_y = (float)height / (float)dst_height ;

  foreach (y=0 ... dst_height, x=0 ... dst_width)
    {
      // source
      varying int sx = round(scale_x * (float)x) ;
      varying int sy = round(scale_y * (float)y) ;
      
      unsigned int src_x = clamp(sx, (varying int32)0, (varying int32)width-1);
      unsigned int src_y = clamp(sy, (varying int32)0, (varying int32)height-1);
      
      unsigned int src_idx = src_y*src_stride+src_x ;
      float tmp = half_to_float_fast(src[src_idx]) ;

      // destination
      unsigned int idx = y * dst_stride + x ;

      //alternative coherent if
      cif (!isnan(tmp))
	    {
        float pixel = 5.0f * (tmp - black) * sensitivity ;
        dst_luma[idx] = pixel > 0.0f ? (int) clamp( 255.0f*pixel/(1.0f + pixel), 0.0f, 255.0f) : 0 ;
        dst_mask[idx] = 255 ;
	    }
      else
      {
        dst_luma[idx] = 0 ;
	      dst_mask[idx] = 0 ;
      }
    } ;  
} ;

export void make_video_frameF16_square(uniform unsigned int16 src[], uniform int width, uniform int height, uniform int src_stride, uniform unsigned int8 dst_luma[], uniform unsigned int8 dst_mask[], uniform int dst_width, uniform int dst_height, uniform int dst_stride, uniform float black, uniform float sensitivity)
{
  uniform float scale_x = (float)width / (float)dst_width ;
  uniform float scale_y = (float)height / (float)dst_height ;

  foreach (y=0 ... dst_height, x=0 ... dst_width)
    {
      // source
      varying int sx = round(scale_x * (float)x) ;
      varying int sy = round(scale_y * (float)y) ;
      
      unsigned int src_x = clamp(sx, (varying int32)0, (varying int32)width-1);
      unsigned int src_y = clamp(sy, (varying int32)0, (varying int32)height-1);
      
      unsigned int src_idx = src_y*src_stride+src_x ;
      float tmp = half_to_float_fast(src[src_idx]) ;

      // destination
      unsigned int idx = y * dst_stride + x ;

      //alternative coherent if
      cif (!isnan(tmp))
	    {
        float pixel = (tmp - black) * sensitivity ;
        dst_luma[idx] = pixel > 0.0f ? (int) clamp( 255.0f*pixel*pixel, 0.0f, 255.0f) : 0 ;
        dst_mask[idx] = 255 ;
	    }
      else
      {
        dst_luma[idx] = 0 ;
	      dst_mask[idx] = 0 ;
      }
    } ;  
} ;

export void make_video_frameF16_legacy(uniform unsigned int16 src[], uniform int width, uniform int height, uniform int src_stride, uniform unsigned int8 dst_luma[], uniform unsigned int8 dst_mask[], uniform int dst_width, uniform int dst_height, uniform int dst_stride, uniform float dmin, uniform float dmax, uniform float lmin, uniform float lmax)
{
  uniform float scale_x = (float)width / (float)dst_width ;
  uniform float scale_y = (float)height / (float)dst_height ;

  foreach (y=0 ... dst_height, x=0 ... dst_width)
    {
      // source
      varying int sx = round(scale_x * (float)x) ;
      varying int sy = round(scale_y * (float)y) ;
      
      unsigned int src_x = clamp(sx, (varying int32)0, (varying int32)width-1);
      unsigned int src_y = clamp(sy, (varying int32)0, (varying int32)height-1);
      
      unsigned int src_idx = src_y*src_stride+src_x ;
      float tmp = half_to_float_fast(src[src_idx]) ;

      // destination
      unsigned int idx = y * dst_stride + x ;

      //alternative coherent if
      cif (!isnan(tmp))
	    {       
        float pixel = 0.5f + (tmp - dmin) / (dmax - dmin) ;
        dst_luma[idx] = pixel > 0.0f ? (int) clamp( 255.0f * (log(pixel) - lmin) / (lmax - lmin), 0.0f, 255.0f) : 0 ;
        dst_mask[idx] = 255 ;
	    }
      else
      {
        dst_luma[idx] = 0 ;
	      dst_mask[idx] = 0 ;
      }
    } ;  
} ;

export uniform float viewport_spectrum_rect(uniform struct fixed_block_t compressed[], uniform int width, uniform int height, uniform float pmin, uniform float pmax, uniform int x1, uniform int x2, uniform int y1, uniform int y2, uniform bool average, uniform double cdelt3)
{
    uniform int cn, cm;

    cn = width / DIM;
    cm = height / DIM;

    // the input dimensions might not be divisible by <DIM>
    if (width % DIM != 0)
        cn = cn + 1;

    if (height % DIM != 0)
        cm = cm + 1;

    uniform int cn_start, cn_end;
    uniform int cm_start, cm_end;

    cn_start = x1 / DIM;
    cn_end = x2 / DIM;
    cm_start = y1 / DIM;
    cm_end = y2 / DIM;

    // print("compressed block range: cn[%,%]; cm[%,%]\n", cn_start, cn_end, cm_start, cm_end);

    varying double partial_sum = 0.0 ;
    varying unsigned int64 partial_count = 0 ;
    
    for(uniform int j=cm_start; j<= cm_end; j++)
      for(uniform int i=cn_start; i <= cn_end; i++)
      { 
        // compressed array entry index
        uniform int src_idx = j * cn + i;

        uniform int emax = (int)compressed[src_idx].common_exp;
        varying float scale = ldexp(1.0f, (uniform float)(emax - significant_bits));

        // block coordinate offset (the same for each point in the block)
        uniform int src_x = i * DIM;
        uniform int src_y = j * DIM;

        // fixed-point decompression
        foreach(k=0 ... BLOCK_SIZE)
        {          
          // find out which FORTRAN column the value is located
          varying int idx = k >> BASE;
          varying int mask = compressed[src_idx].mask[idx];
          varying int pos = k & (DIM -1); // k % DIM
          varying bool isnan = (mask >> pos) & 1;

          // get the coordinates within a DIMxDIM block
          varying int pixel_x = src_x + pos; // ix = pos
          varying int pixel_y = src_y + idx; // iy = idx;

          // check if a pixel is not a NaN and if it lies within the viewport window
          varying bool valid = !isnan && (pixel_x >= x1) && (pixel_x <= x2) && (pixel_y >= y1) && (pixel_y <= y2);          

          cif(valid)
          {            
            varying float pixel = scale * (float)compressed[src_idx].mantissa[k];

            // recover the original range            
            partial_sum += pixel; // pmin + (exp(pixel) - 0.5f)*(pmax - pmin);
            partial_count += isnan ? 0 : 1;
          }      
        }
      }

    uniform double sum = reduce_add(partial_sum) ;
    uniform int64 count = reduce_add(partial_count) ;

    if(count > 0)
    {
        if(average)
	        return (float)(sum / (double) count) ;
        else
	        return (float)(sum * cdelt3) ;
    }
    else
        return 0.0f ;
}

export uniform float viewport_spectrum_circle(uniform struct fixed_block_t compressed[], uniform int width, uniform int height, uniform float pmin, uniform float pmax, uniform int x1, uniform int x2, uniform int y1, uniform int y2, uniform float cx, uniform float cy, uniform float r2, uniform bool average, uniform double cdelt3)
{
    uniform int cn, cm;

    cn = width / DIM;
    cm = height / DIM;

    // the input dimensions might not be divisible by <DIM>
    if (width % DIM != 0)
        cn = cn + 1;

    if (height % DIM != 0)
        cm = cm + 1;

    uniform int cn_start, cn_end;
    uniform int cm_start, cm_end;

    cn_start = x1 / DIM;
    cn_end = x2 / DIM;
    cm_start = y1 / DIM;
    cm_end = y2 / DIM;

    // print("compressed block range: cn[%,%]; cm[%,%]\n", cn_start, cn_end, cm_start, cm_end);

    varying double partial_sum = 0.0 ;
    varying unsigned int64 partial_count = 0 ;
    
    for(uniform int j=cm_start; j<= cm_end; j++)
      for(uniform int i=cn_start; i <= cn_end; i++)
      { 
        // compressed array entry index
        uniform int src_idx = j * cn + i;

        uniform int emax = (int)compressed[src_idx].common_exp;
        varying float scale = ldexp(1.0f, (uniform float)(emax - significant_bits));

        // block coordinate offset (the same for each point in the block)
        uniform int src_x = i * DIM;
        uniform int src_y = j * DIM;

        // fixed-point decompression
        foreach(k=0 ... BLOCK_SIZE)
        {          
          // find out which FORTRAN column the value is located
          varying int idx = k >> BASE;
          varying int mask = compressed[src_idx].mask[idx];
          varying int pos = k & (DIM -1); // k % DIM
          varying bool isnan = (mask >> pos) & 1;

          // get the coordinates within a DIMxDIM block
          varying int pixel_x = src_x + pos; // ix = pos
          varying int pixel_y = src_y + idx; // iy = idx;
          varying float dist2 = (cx-pixel_x)*(cx-pixel_x) + (cy-pixel_y)*(cy-pixel_y) ;

          // check if a pixel is not a NaN and if it lies within the viewport circle
          varying bool valid = !isnan && (dist2 <= r2);
                     
          cif(valid)
          {            
            varying float pixel = scale * (float)compressed[src_idx].mantissa[k];

            // recover the original range            
            partial_sum += pixel; // pmin + (exp(pixel) - 0.5f)*(pmax - pmin);
            partial_count += isnan ? 0 : 1;
          }      
        }
      }

    uniform double sum = reduce_add(partial_sum) ;
    uniform int64 count = reduce_add(partial_count) ;

    if(count > 0)
    {
        if(average)
	        return (float)(sum / (double) count) ;
        else
	        return (float)(sum * cdelt3) ;
    }
    else
        return 0.0f ;
}

export uniform float viewport_image_spectrum_rect(uniform struct fixed_block_t compressed[], uniform int width, uniform int height, uniform float pmin, uniform float pmax, uniform float view_pixels[], uniform bool view_mask[], uniform int stride, uniform int x1, uniform int x2, uniform int y1, uniform int y2, uniform int horizontal, uniform int vertical, uniform bool average, uniform double cdelt3, uniform float median, uniform float sumP[], uniform int64 countP[], uniform float sumN[], uniform int64 countN[])
{
    uniform int cn, cm;

    cn = width / DIM;
    cm = height / DIM;

    // the input dimensions might not be divisible by <DIM>
    if (width % DIM != 0)
        cn = cn + 1;

    if (height % DIM != 0)
        cm = cm + 1;

    uniform int cn_start, cn_end;
    uniform int cm_start, cm_end;

    cn_start = x1 / DIM;
    cn_end = x2 / DIM;
    cm_start = y1 / DIM;
    cm_end = y2 / DIM;

    // print("compressed block range: cn[%,%]; cm[%,%]\n", cn_start, cn_end, cm_start, cm_end);

    varying double partial_sum = 0.0 ;
    varying unsigned int64 partial_count = 0 ;

    varying double partial_sumP = 0.0 ;
    varying unsigned int64 partial_countP = 0 ;

    varying double partial_sumN = 0.0 ;
    varying unsigned int64 partial_countN = 0 ;

    varying bool do_stat = !isnan(median);

    for(uniform int j=cm_start; j<= cm_end; j++)
      for(uniform int i=cn_start; i <= cn_end; i++)
      { 
        // compressed array entry index
        uniform int src_idx = j * cn + i;

        uniform int emax = (int)compressed[src_idx].common_exp;
        varying float scale = ldexp(1.0f, (uniform float)(emax - significant_bits));

        // block coordinate offset (the same for each point in the block)
        uniform int src_x = i * DIM;
        uniform int src_y = j * DIM;
        
        // fixed-point decompression
        foreach(k=0 ... BLOCK_SIZE)
        {          
          // find out which FORTRAN column the value is located
          varying int idx = k >> BASE;
          varying int mask = compressed[src_idx].mask[idx];
          varying int pos = k & (DIM -1); // k % DIM
          varying bool isnan = (mask >> pos) & 1;

          // get the coordinates within a DIMxDIM block
          varying int pixel_x = src_x + pos; // ix = pos
          varying int pixel_y = src_y + idx; // iy = idx;

          varying int offset_x = pixel_x - x1 + horizontal;
          varying int offset_y = pixel_y - y1 + vertical;
          varying int offset = offset_y*stride + offset_x;

          // check if a pixel is not a NaN and if it lies within the viewport window
          varying bool valid = !isnan && (pixel_x >= x1) && (pixel_x <= x2) && (pixel_y >= y1) && (pixel_y <= y2);          

          cif(valid)
          {                        
            varying float tmp = scale * (float)compressed[src_idx].mantissa[k];

            // recover the original range
            // tmp = pmin + (exp(tmp) - 0.5f)*(pmax - pmin);

            partial_sum += tmp;
            partial_count += isnan ? 0 : 1;

            view_pixels[offset] += tmp * cdelt3 ;            
            view_mask[offset] |= isnan ? false : true ;

            cif(do_stat)
            {
              varying float deviation = abs(tmp - median); // absolute deviation from the median

              // the '=' sign is to make sure datasets with all pixels being identical are handled too            
              partial_sumP += (tmp >= median) ? deviation : 0.0f;
              partial_sumN += (tmp < median) ? deviation : 0.0f;

              // increment the counters
              partial_countP += (tmp >= median) ? 1 : 0;
              partial_countN += (tmp < median) ? 1 : 0;
            }
          }      
        }
      }

    // the final reductions
    uniform double sum = reduce_add(partial_sum) ;
    uniform int64 count = reduce_add(partial_count) ;

    if(do_stat)
    {
      sumP[0] += (float)reduce_add(partial_sumP) ;
      countP[0] += reduce_add(partial_countP) ;
      sumN[0] += (float)reduce_add(partial_sumN) ;
      countN[0] += reduce_add(partial_countN) ;
    }

    if(count > 0)
    {
        if(average)
	        return (float)(sum / (double) count) ;
        else
	        return (float)(sum * cdelt3) ;
    }
    else
        return 0.0f ;
}

export uniform float viewport_image_spectrum_circle(uniform struct fixed_block_t compressed[], uniform int width, uniform int height, uniform float pmin, uniform float pmax, uniform float view_pixels[], uniform bool view_mask[], uniform int stride, uniform int x1, uniform int x2, uniform int y1, uniform int y2, uniform int horizontal, uniform int vertical, uniform float cx, uniform float cy, uniform float r2, uniform bool average, uniform double cdelt3)
{
    uniform int cn, cm;

    cn = width / DIM;
    cm = height / DIM;

    // the input dimensions might not be divisible by <DIM>
    if (width % DIM != 0)
        cn = cn + 1;

    if (height % DIM != 0)
        cm = cm + 1;

    uniform int cn_start, cn_end;
    uniform int cm_start, cm_end;

    cn_start = x1 / DIM;
    cn_end = x2 / DIM;
    cm_start = y1 / DIM;
    cm_end = y2 / DIM;

    // print("compressed block range: cn[%,%]; cm[%,%]\n", cn_start, cn_end, cm_start, cm_end);    

    varying double partial_sum = 0.0 ;
    varying unsigned int64 partial_count = 0 ;
    
    for(uniform int j=cm_start; j<= cm_end; j++)
      for(uniform int i=cn_start; i <= cn_end; i++)
      { 
        // compressed array entry index
        uniform int src_idx = j * cn + i;

        uniform int emax = (int)compressed[src_idx].common_exp;
        varying float scale = ldexp(1.0f, (uniform float)(emax - significant_bits));

        // block coordinate offset (the same for each point in the block)
        uniform int src_x = i * DIM;
        uniform int src_y = j * DIM;

        // fixed-point decompression
        foreach(k=0 ... BLOCK_SIZE)
        {          
          // find out which FORTRAN column the value is located
          varying int idx = k >> BASE;
          varying int mask = compressed[src_idx].mask[idx];
          varying int pos = k & (DIM -1); // k % DIM
          varying bool isnan = (mask >> pos) & 1;

          // get the coordinates within a DIMxDIM block
          varying int pixel_x = src_x + pos; // ix = pos
          varying int pixel_y = src_y + idx; // iy = idx;
          varying float dist2 = (cx-pixel_x)*(cx-pixel_x) + (cy-pixel_y)*(cy-pixel_y) ;
          
          varying int offset_x = pixel_x - x1 + horizontal;
          varying int offset_y = pixel_y - y1 + vertical;
          varying int offset = offset_y*stride + offset_x;

          // compute the underlying floating-point value
          varying float tmp = scale * (float)compressed[src_idx].mantissa[k];

          // recover the original range
          // tmp = pmin + (exp(tmp) - 0.5f)*(pmax - pmin);

          // check if a pixel is not a NaN and if it lies within the viewport rectangular window
          varying bool valid = !isnan && (pixel_x >= x1) && (pixel_x <= x2) && (pixel_y >= y1) && (pixel_y <= y2);          

          // the viewport (use the square, JavaScript will truncate / turn it into a circle)
          cif(valid)
          {                        
            view_pixels[offset] += tmp * cdelt3 ;
            view_mask[offset] |= isnan ? false : true ;
          }      

          // further restrict the pixels to within the viewport circle
          valid = valid && (dist2 <= r2);
                     
          // the spectrum
          cif(valid)
          {                        
            partial_sum += tmp;
            partial_count += isnan ? 0 : 1;            
          }      
        }
      }

    uniform double sum = reduce_add(partial_sum) ;
    uniform int64 count = reduce_add(partial_count) ;

    if(count > 0)
    {
        if(average)
	        return (float)(sum / (double) count) ;
        else
	        return (float)(sum * cdelt3) ;
    }
    else
        return 0.0f ;
}

export void make_global_statistics(uniform struct fixed_block_t compressed[], uniform int width, uniform int height, uniform float median, uniform float sumP[], uniform int64 countP[], uniform float sumN[], uniform int64 countN[])
{
  uniform int cn, cm;

  cn = width / DIM;
  cm = height / DIM;

  // the input dimensions might not be divisible by <DIM>
  if (width % DIM != 0)
    cn = cn + 1;

  if (height % DIM != 0)
    cm = cm + 1;

  varying double partial_sumP = 0.0 ;
  varying unsigned int64 partial_countP = 0 ;

  varying double partial_sumN = 0.0 ;
  varying unsigned int64 partial_countN = 0 ;

  // varying float max_value = 0.0f ;

  // go through the entire plane
  for(uniform int j=0; j<cm; j++)
    for(uniform int i=0; i <cn; i++)
    { 
      // compressed array entry index
      uniform int src_idx = j * cn + i;

      uniform int emax = (int)compressed[src_idx].common_exp;
      varying float scale = ldexp(1.0f, (uniform float)(emax - significant_bits));

      foreach(k=0 ... BLOCK_SIZE)
      {          
          // find out which FORTRAN column the value is located
          varying int idx = k >> BASE;
          varying int mask = compressed[src_idx].mask[idx];
          varying int pos = k & (DIM -1); // k % DIM
          varying bool isnan = (mask >> pos) & 1;

          // calculate the mean absolute deviation from the median
          cif(!isnan)
          {
            varying float x = scale * (float)compressed[src_idx].mantissa[k];
            varying float tmp = abs(x - median); // absolute deviation from the median

            // the '=' sign is to make sure datasets with all pixels being identical are handled too            
            // disabled the '=' sign after all (the sum countP + countN must add up to the total number of pixels!)
            
            partial_sumP += (x >= median) ? tmp : 0.0f;
            partial_sumN += (x < median) ? tmp : 0.0f;

            // increment the counters
            partial_countP += (x >= median) ? 1 : 0;
            partial_countN += (x < median) ? 1 : 0;

            /*if(x > max_value)
              max_value = x;*/
          }

        }

    }  

  // the final reductions
  sumP[0] += (float)reduce_add(partial_sumP) ;
  countP[0] += reduce_add(partial_countP) ;
  sumN[0] += (float)reduce_add(partial_sumN) ;
  countN[0] += reduce_add(partial_countN) ;

  // print("median: %, sumP: %, sumN: %, countP: %, countN: %, max_value: %\n", median, sumP[0], sumN[0], countP[0], countN[0], reduce_max(max_value));
}

export void make_video_frame_fixed_linear(uniform struct fixed_block_t compressed[], uniform int width, uniform int height, uniform unsigned int8 dst_luma[], uniform unsigned int8 dst_mask[], uniform int stride, uniform float black, uniform float slope)
{
  uniform int cn, cm;

  cn = width / DIM;
  cm = height / DIM;

  // the input dimensions might not be divisible by <DIM>
  if (width % DIM != 0)
    cn = cn + 1;

  if (height % DIM != 0)
    cm = cm + 1;

  // go through the entire plane
  for(uniform int j=0; j<cm; j++)
    for(uniform int i=0; i <cn; i++)
    { 
      // compressed array entry index
      uniform int src_idx = j * cn + i;

      uniform int emax = (int)compressed[src_idx].common_exp;
      varying float scale = ldexp(1.0f, (uniform float)(emax - significant_bits));

      // block coordinate offset (the same for each point in the block)
      uniform int src_x = i * DIM;
      uniform int src_y = j * DIM;

      foreach(k=0 ... BLOCK_SIZE)
      {          
          // find out which FORTRAN column the value is located
          varying int idx = k >> BASE;
          varying int mask = compressed[src_idx].mask[idx];
          varying int pos = k & (DIM -1); // k % DIM
          varying bool isnan = (mask >> pos) & 1;

          // get the coordinates within a DIMxDIM block
          varying int pixel_x = src_x + pos; // ix = pos
          varying int pixel_y = src_y + idx; // iy = idx;

          // destination index
          varying int offset = pixel_y*stride + pixel_x;

          varying bool valid = !isnan && (pixel_x < width) && (pixel_y < height);

          // apply tone mapping
          cif(valid)
          {
            varying float x = scale * (float)compressed[src_idx].mantissa[k];
            varying float pixel = 255.0f / ( 1.0f + exp(-6.0f * (x - black) * slope) ) ;

            dst_luma[offset] = (int) clamp(pixel, 0.0f, 255.0f) ;
            dst_mask[offset] = 255 ;      
          }          

        }

    }

}

export void make_video_frame_fixed_linear_threaded(uniform struct fixed_block_t compressed[], uniform int width, uniform int height, uniform unsigned int8 dst_luma[], uniform unsigned int8 dst_mask[], uniform int stride, uniform float black, uniform float slope, uniform int start, uniform int work_size)
{
  uniform int cn = width / DIM;  

  // the input dimensions might not be divisible by <DIM>
  if (width % DIM != 0)
    cn = cn + 1;

  // go through the entire plane
  for(uniform int j=start; j<start+work_size; j++)
    for(uniform int i=0; i <cn; i++)
    { 
      // compressed array entry index
      uniform int src_idx = j * cn + i;

      uniform int emax = (int)compressed[src_idx].common_exp;
      varying float scale = ldexp(1.0f, (uniform float)(emax - significant_bits));

      // block coordinate offset (the same for each point in the block)
      uniform int src_x = i * DIM;
      uniform int src_y = j * DIM;

      foreach(k=0 ... BLOCK_SIZE)
      {          
          // find out which FORTRAN column the value is located
          varying int idx = k >> BASE;
          varying int mask = compressed[src_idx].mask[idx];
          varying int pos = k & (DIM -1); // k % DIM
          varying bool isnan = (mask >> pos) & 1;

          // get the coordinates within a DIMxDIM block
          varying int pixel_x = src_x + pos; // ix = pos
          varying int pixel_y = src_y + idx; // iy = idx;

          // destination index
          varying int offset = pixel_y*stride + pixel_x;

          varying bool valid = !isnan && (pixel_x < width) && (pixel_y < height);

          // apply tone mapping
          cif(valid)
          {
            varying float x = scale * (float)compressed[src_idx].mantissa[k];
            varying float pixel = 255.0f / ( 1.0f + exp(-6.0f * (x - black) * slope) ) ;

            dst_luma[offset] = (int) clamp(pixel, 0.0f, 255.0f) ;
            dst_mask[offset] = 255 ;      
          }          

        }

    }

}

export void make_video_frame_fixed_logistic(uniform struct fixed_block_t compressed[], uniform int width, uniform int height, uniform unsigned int8 dst_luma[], uniform unsigned int8 dst_mask[], uniform int stride, uniform float median, uniform float sensitivity)
{
  uniform int cn, cm;

  cn = width / DIM;
  cm = height / DIM;

  // the input dimensions might not be divisible by <DIM>
  if (width % DIM != 0)
    cn = cn + 1;

  if (height % DIM != 0)
    cm = cm + 1;

  // go through the entire plane
  for(uniform int j=0; j<cm; j++)
    for(uniform int i=0; i <cn; i++)
    { 
      // compressed array entry index
      uniform int src_idx = j * cn + i;

      uniform int emax = (int)compressed[src_idx].common_exp;
      varying float scale = ldexp(1.0f, (uniform float)(emax - significant_bits));

      // block coordinate offset (the same for each point in the block)
      uniform int src_x = i * DIM;
      uniform int src_y = j * DIM;

      foreach(k=0 ... BLOCK_SIZE)
      {          
          // find out which FORTRAN column the value is located
          varying int idx = k >> BASE;
          varying int mask = compressed[src_idx].mask[idx];
          varying int pos = k & (DIM -1); // k % DIM
          varying bool isnan = (mask >> pos) & 1;

          // get the coordinates within a DIMxDIM block
          varying int pixel_x = src_x + pos; // ix = pos
          varying int pixel_y = src_y + idx; // iy = idx;

          // destination index
          varying int offset = pixel_y*stride + pixel_x;

          varying bool valid = !isnan && (pixel_x < width) && (pixel_y < height);

          // apply tone mapping
          cif(valid)
          {
            varying float x = scale * (float)compressed[src_idx].mantissa[k];            
            varying float pixel = 255.0f / ( 1.0f + exp(-6.0f * (x - median) * sensitivity) ) ;

            dst_luma[offset] = (int) clamp(pixel, 0.0f, 255.0f) ;
            dst_mask[offset] = 255 ;      
          }          

        }

    }
    
}

export void make_video_frame_fixed_logistic_threaded(uniform struct fixed_block_t compressed[], uniform int width, uniform int height, uniform unsigned int8 dst_luma[], uniform unsigned int8 dst_mask[], uniform int stride, uniform float median, uniform float sensitivity, uniform int start, uniform int work_size)
{
  uniform int cn = width / DIM;  

  // the input dimensions might not be divisible by <DIM>
  if (width % DIM != 0)
    cn = cn + 1;

  // go through the entire plane
  for(uniform int j=start; j<start+work_size; j++)
    for(uniform int i=0; i <cn; i++)
    { 
      // compressed array entry index
      uniform int src_idx = j * cn + i;

      uniform int emax = (int)compressed[src_idx].common_exp;
      varying float scale = ldexp(1.0f, (uniform float)(emax - significant_bits));

      // block coordinate offset (the same for each point in the block)
      uniform int src_x = i * DIM;
      uniform int src_y = j * DIM;

      foreach(k=0 ... BLOCK_SIZE)
      {          
          // find out which FORTRAN column the value is located
          varying int idx = k >> BASE;
          varying int mask = compressed[src_idx].mask[idx];
          varying int pos = k & (DIM -1); // k % DIM
          varying bool isnan = (mask >> pos) & 1;

          // get the coordinates within a DIMxDIM block
          varying int pixel_x = src_x + pos; // ix = pos
          varying int pixel_y = src_y + idx; // iy = idx;

          // destination index
          varying int offset = pixel_y*stride + pixel_x;

          varying bool valid = !isnan && (pixel_x < width) && (pixel_y < height);

          // apply tone mapping
          cif(valid)
          {
            varying float x = scale * (float)compressed[src_idx].mantissa[k];            
            varying float pixel = 255.0f / ( 1.0f + exp(-6.0f * (x - median) * sensitivity) ) ;

            dst_luma[offset] = (int) clamp(pixel, 0.0f, 255.0f) ;
            dst_mask[offset] = 255 ;      
          }          

        }

    }
    
}

export void make_video_frame_fixed_ratio(uniform struct fixed_block_t compressed[], uniform int width, uniform int height, uniform unsigned int8 dst_luma[], uniform unsigned int8 dst_mask[], uniform int stride, uniform float black, uniform float sensitivity)
{
  uniform int cn, cm;

  cn = width / DIM;
  cm = height / DIM;

  // the input dimensions might not be divisible by <DIM>
  if (width % DIM != 0)
    cn = cn + 1;

  if (height % DIM != 0)
    cm = cm + 1;

    // go through the entire plane
  for(uniform int j=0; j<cm; j++)
    for(uniform int i=0; i <cn; i++)
    { 
      // compressed array entry index
      uniform int src_idx = j * cn + i;

      uniform int emax = (int)compressed[src_idx].common_exp;
      varying float scale = ldexp(1.0f, (uniform float)(emax - significant_bits));

      // block coordinate offset (the same for each point in the block)
      uniform int src_x = i * DIM;
      uniform int src_y = j * DIM;

      foreach(k=0 ... BLOCK_SIZE)
      {          
          // find out which FORTRAN column the value is located
          varying int idx = k >> BASE;
          varying int mask = compressed[src_idx].mask[idx];
          varying int pos = k & (DIM -1); // k % DIM
          varying bool isnan = (mask >> pos) & 1;

          // get the coordinates within a DIMxDIM block
          varying int pixel_x = src_x + pos; // ix = pos
          varying int pixel_y = src_y + idx; // iy = idx;

          // destination index
          varying int offset = pixel_y*stride + pixel_x;

          varying bool valid = !isnan && (pixel_x < width) && (pixel_y < height);

          // apply tone mapping
          cif(valid)
          {
            varying float x = scale * (float)compressed[src_idx].mantissa[k];            
            varying float pixel = 5.0f * (x - black) * sensitivity ;

            dst_luma[offset] = pixel > 0.0f ? (int) clamp( 255.0f*pixel/(1.0f + pixel), 0.0f, 255.0f) : 0 ;
            dst_mask[offset] = 255 ;      
          }          

        }

    }
    
}

export void make_video_frame_fixed_ratio_threaded(uniform struct fixed_block_t compressed[], uniform int width, uniform int height, uniform unsigned int8 dst_luma[], uniform unsigned int8 dst_mask[], uniform int stride, uniform float black, uniform float sensitivity, uniform int start, uniform int work_size)
{  
  uniform int cn = width / DIM;

  // the input dimensions might not be divisible by <DIM>
  if (width % DIM != 0)
    cn = cn + 1;

  // go through the entire plane
  for(uniform int j=start; j<start+work_size; j++)
    for(uniform int i=0; i <cn; i++)
    { 
      // compressed array entry index
      uniform int src_idx = j * cn + i;

      uniform int emax = (int)compressed[src_idx].common_exp;
      varying float scale = ldexp(1.0f, (uniform float)(emax - significant_bits));

      // block coordinate offset (the same for each point in the block)
      uniform int src_x = i * DIM;
      uniform int src_y = j * DIM;

      foreach(k=0 ... BLOCK_SIZE)
      {          
          // find out which FORTRAN column the value is located
          varying int idx = k >> BASE;
          varying int mask = compressed[src_idx].mask[idx];
          varying int pos = k & (DIM -1); // k % DIM
          varying bool isnan = (mask >> pos) & 1;

          // get the coordinates within a DIMxDIM block
          varying int pixel_x = src_x + pos; // ix = pos
          varying int pixel_y = src_y + idx; // iy = idx;

          // destination index
          varying int offset = pixel_y*stride + pixel_x;

          varying bool valid = !isnan && (pixel_x < width) && (pixel_y < height);

          // apply tone mapping
          cif(valid)
          {
            varying float x = scale * (float)compressed[src_idx].mantissa[k];            
            varying float pixel = 5.0f * (x - black) * sensitivity ;

            dst_luma[offset] = pixel > 0.0f ? (int) clamp( 255.0f*pixel/(1.0f + pixel), 0.0f, 255.0f) : 0 ;
            dst_mask[offset] = 255 ;      
          }          

        }

    }
    
}

export void make_video_frame_fixed_square(uniform struct fixed_block_t compressed[], uniform int width, uniform int height, uniform unsigned int8 dst_luma[], uniform unsigned int8 dst_mask[], uniform int stride, uniform float black, uniform float sensitivity)
{
  uniform int cn, cm;

  cn = width / DIM;
  cm = height / DIM;

  // the input dimensions might not be divisible by <DIM>
  if (width % DIM != 0)
    cn = cn + 1;

  if (height % DIM != 0)
    cm = cm + 1;

    // go through the entire plane
  for(uniform int j=0; j<cm; j++)
    for(uniform int i=0; i <cn; i++)
    { 
      // compressed array entry index
      uniform int src_idx = j * cn + i;

      uniform int emax = (int)compressed[src_idx].common_exp;
      varying float scale = ldexp(1.0f, (uniform float)(emax - significant_bits));

      // block coordinate offset (the same for each point in the block)
      uniform int src_x = i * DIM;
      uniform int src_y = j * DIM;

      foreach(k=0 ... BLOCK_SIZE)
      {          
          // find out which FORTRAN column the value is located
          varying int idx = k >> BASE;
          varying int mask = compressed[src_idx].mask[idx];
          varying int pos = k & (DIM -1); // k % DIM
          varying bool isnan = (mask >> pos) & 1;

          // get the coordinates within a DIMxDIM block
          varying int pixel_x = src_x + pos; // ix = pos
          varying int pixel_y = src_y + idx; // iy = idx;

          // destination index
          varying int offset = pixel_y*stride + pixel_x;

          varying bool valid = !isnan && (pixel_x < width) && (pixel_y < height);

          // apply tone mapping
          cif(valid)
          {
            varying float x = scale * (float)compressed[src_idx].mantissa[k];
            varying float pixel = (x - black) * sensitivity ;
            
            dst_luma[offset] = pixel > 0.0f ? (int) clamp( 255.0f*pixel*pixel, 0.0f, 255.0f) : 0 ;            
            dst_mask[offset] = 255 ;    
          }          

        }

    }
    
}

export void make_video_frame_fixed_square_threaded(uniform struct fixed_block_t compressed[], uniform int width, uniform int height, uniform unsigned int8 dst_luma[], uniform unsigned int8 dst_mask[], uniform int stride, uniform float black, uniform float sensitivity, uniform int start, uniform int work_size)
{
  uniform int cn = width / DIM;

  // the input dimensions might not be divisible by <DIM>
  if (width % DIM != 0)
    cn = cn + 1;

  // go through the entire plane
  for(uniform int j=start; j<start+work_size; j++)
    for(uniform int i=0; i <cn; i++)
    { 
      // compressed array entry index
      uniform int src_idx = j * cn + i;

      uniform int emax = (int)compressed[src_idx].common_exp;
      varying float scale = ldexp(1.0f, (uniform float)(emax - significant_bits));

      // block coordinate offset (the same for each point in the block)
      uniform int src_x = i * DIM;
      uniform int src_y = j * DIM;

      foreach(k=0 ... BLOCK_SIZE)
      {          
          // find out which FORTRAN column the value is located
          varying int idx = k >> BASE;
          varying int mask = compressed[src_idx].mask[idx];
          varying int pos = k & (DIM -1); // k % DIM
          varying bool isnan = (mask >> pos) & 1;

          // get the coordinates within a DIMxDIM block
          varying int pixel_x = src_x + pos; // ix = pos
          varying int pixel_y = src_y + idx; // iy = idx;

          // destination index
          varying int offset = pixel_y*stride + pixel_x;

          varying bool valid = !isnan && (pixel_x < width) && (pixel_y < height);

          // apply tone mapping
          cif(valid)
          {
            varying float x = scale * (float)compressed[src_idx].mantissa[k];
            varying float pixel = (x - black) * sensitivity ;
            
            dst_luma[offset] = pixel > 0.0f ? (int) clamp( 255.0f*pixel*pixel, 0.0f, 255.0f) : 0 ;            
            dst_mask[offset] = 255 ;    
          }          

        }

    }
    
}

export void make_video_frame_fixed_legacy(uniform struct fixed_block_t compressed[], uniform int width, uniform int height, uniform unsigned int8 dst_luma[], uniform unsigned int8 dst_mask[], uniform int stride, uniform float dmin, uniform float dmax, uniform float lmin, uniform float lmax)
{
  uniform int cn, cm;

  cn = width / DIM;
  cm = height / DIM;

  // the input dimensions might not be divisible by <DIM>
  if (width % DIM != 0)
    cn = cn + 1;

  if (height % DIM != 0)
    cm = cm + 1;

  // go through the entire plane
  for(uniform int j=0; j<cm; j++)
    for(uniform int i=0; i <cn; i++)
    { 
      // compressed array entry index
      uniform int src_idx = j * cn + i;

      uniform int emax = (int)compressed[src_idx].common_exp;
      varying float scale = ldexp(1.0f, (uniform float)(emax - significant_bits));

      // block coordinate offset (the same for each point in the block)
      uniform int src_x = i * DIM;
      uniform int src_y = j * DIM;

      foreach(k=0 ... BLOCK_SIZE)
      {          
          // find out which FORTRAN column the value is located
          varying int idx = k >> BASE;
          varying int mask = compressed[src_idx].mask[idx];
          varying int pos = k & (DIM -1); // k % DIM
          varying bool isnan = (mask >> pos) & 1;

          // get the coordinates within a DIMxDIM block
          varying int pixel_x = src_x + pos; // ix = pos
          varying int pixel_y = src_y + idx; // iy = idx;

          // destination index
          varying int offset = pixel_y*stride + pixel_x;

          varying bool valid = !isnan && (pixel_x < width) && (pixel_y < height);

          // apply tone mapping
          cif(valid)
          {
            varying float x = scale * (float)compressed[src_idx].mantissa[k];
            varying float pixel = 0.5f + (x - dmin) / (dmax - dmin) ;
        
            dst_luma[offset] = pixel > 0.0f ? (int) clamp( 255.0f * (log(pixel) - lmin) / (lmax - lmin), 0.0f, 255.0f) : 0 ;        
            dst_mask[offset] = 255 ;
          }          

        }

    }
    
}

export void make_video_frame_fixed_legacy_threaded(uniform struct fixed_block_t compressed[], uniform int width, uniform int height, uniform unsigned int8 dst_luma[], uniform unsigned int8 dst_mask[], uniform int stride, uniform float dmin, uniform float dmax, uniform float lmin, uniform float lmax, uniform int start, uniform int work_size)
{
  uniform int cn = width / DIM;

  // the input dimensions might not be divisible by <DIM>
  if (width % DIM != 0)
    cn = cn + 1;

  // go through the entire plane
  for(uniform int j=start; j<start+work_size; j++)
    for(uniform int i=0; i <cn; i++)
    { 
      // compressed array entry index
      uniform int src_idx = j * cn + i;

      uniform int emax = (int)compressed[src_idx].common_exp;
      varying float scale = ldexp(1.0f, (uniform float)(emax - significant_bits));

      // block coordinate offset (the same for each point in the block)
      uniform int src_x = i * DIM;
      uniform int src_y = j * DIM;

      foreach(k=0 ... BLOCK_SIZE)
      {          
          // find out which FORTRAN column the value is located
          varying int idx = k >> BASE;
          varying int mask = compressed[src_idx].mask[idx];
          varying int pos = k & (DIM -1); // k % DIM
          varying bool isnan = (mask >> pos) & 1;

          // get the coordinates within a DIMxDIM block
          varying int pixel_x = src_x + pos; // ix = pos
          varying int pixel_y = src_y + idx; // iy = idx;

          // destination index
          varying int offset = pixel_y*stride + pixel_x;

          varying bool valid = !isnan && (pixel_x < width) && (pixel_y < height);

          // apply tone mapping
          cif(valid)
          {
            varying float x = scale * (float)compressed[src_idx].mantissa[k];
            varying float pixel = 0.5f + (x - dmin) / (dmax - dmin) ;
        
            dst_luma[offset] = pixel > 0.0f ? (int) clamp( 255.0f * (log(pixel) - lmin) / (lmax - lmin), 0.0f, 255.0f) : 0 ;        
            dst_mask[offset] = 255 ;
          }          

        }

    }
    
}

export void resizeNearestSIMD(uniform unsigned int8 src[], uniform int srcWidth, uniform int srcHeight, uniform unsigned int8 dst[], uniform int dstWidth, uniform int dstHeight)
{
  const uniform int x_ratio = (int)((srcWidth << 16) / dstWidth) + 1;
  const uniform int y_ratio = (int)((srcHeight << 16) / dstHeight) + 1;

  foreach (i = 0 ... dstHeight, j = 0 ... dstWidth)
  {    
    varying int x2 = (j * x_ratio)  >> 16;
    varying int y2 = (i * y_ratio) >> 16;
    dst[i * dstWidth + j] = src[y2 * srcWidth + x2];
  }
}

export void fits2uint8(uniform unsigned int8 src[], uniform float dest[], uniform unsigned int size)
{
  foreach(i=0 ... size)
    {
      // no endian conversion
      dest[i] = (float)(src[i]) ;      
    } ;
} ;

export void fits2int16(uniform int16 src[], uniform float dest[], uniform unsigned int size)
{
  foreach(i=0 ... size)
    {
      //endian conversion
      dest[i] = (float)((int16)(bswap16(src[i]))) ;
    } ;
} ;

export void fits2int32(uniform int32 src[], uniform float dest[], uniform unsigned int size)
{
  foreach(i=0 ... size)
    {
      //endian conversion
      dest[i] = (float)((int32)(bswap32(src[i]))) ;
    } ;
} ;

export void fits2int64(uniform int64 src[], uniform float dest[], uniform unsigned int size)
{
  foreach(i=0 ... size)
    {
      //endian conversion
      dest[i] = (float)((int64)(bswap64(src[i]))) ;
    } ;
} ;

export void fits2float32(uniform int32 src[], uniform float dest[], uniform unsigned int size)
{
  foreach(i=0 ... size)
    {
      //endian conversion
      dest[i] = floatbits(bswap32(src[i])) ;      
    } ;
} ;

export uniform bool fits2float64(uniform int64 src[], uniform float dest[], uniform unsigned int size)
{
    varying bool error = false ;

  foreach(i=0 ... size)
    {
      //endian conversion
      double tmp = doublebits(bswap64(src[i])) ;

        // the range should be checked here in the first place (coercing double -> float)
        if(tmp > FLT_MAX)
        {
            unsigned int inf = 0x7f800000; // +Infinity
            error = true ;
            dest[i] = floatbits(inf);
        } else
        if(tmp < -FLT_MAX)
        {
            unsigned int inf = 0xff800000; // -Infinity
            error = true ;
            dest[i] = floatbits(inf);
        }
        else
        {    
            dest[i] = (float)tmp;
        }
    } ;

    return any(error) ;
} ;