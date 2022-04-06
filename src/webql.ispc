typedef int16 int16_t;
typedef int32 int32_t;
typedef int64 int64_t;
typedef unsigned int16 uint16_t;
typedef unsigned int32 uint32_t;
typedef unsigned int64 uint64_t;

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

bool isinf(float tmp)
{
  unsigned int i = intbits(tmp) ;
  return ( (i & 0x7fffffff) == 0x7f800000 ) || ( (i & 0xffffffff) == 0xff800000 ) ;
}

export void  make_image_spectrumF32(uniform float src[], uniform float pixels[], uniform unsigned int8 mask[], uniform unsigned int8 data_mask[], uniform float ignrval, uniform float datamin, uniform float datamax,  uniform float cdelt3, uniform float res[], uniform int npixels)
{
  uniform float frame_min, frame_max, mean, integrated;

  frame_min = res[0];
  frame_max = res[1];

  varying float partial_sum = 0.0f ;
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

uniform float sum = reduce_add(partial_sum) ;
uniform int64 count = reduce_add(partial_count) ;

  if(count > 0)
    {

      mean = sum / (float) count ;
      integrated = sum * cdelt3 ;
    }
  else
    {      
      mean = 0.0f ;
      integrated = 0.0f ;
    }

  // print ("mean: %, integrated: %\n", mean, integrated);

  frame_min = reduce_min(vmin) ;
  frame_max = reduce_max(vmax) ;

  res[0] = frame_min;
  res[1] = frame_max;
  res[2] = mean;
  res[3] = integrated;
} ;

export uniform float calculate_radial_spectrumF16(uniform int16 cubeData[], uniform unsigned int stride, uniform int x1, uniform int x2, uniform int y1, uniform int y2, uniform int cx, uniform int cy, uniform int r2, uniform bool average, uniform float cdelt3)
{  
  float partial_sum = 0.0f ;
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

  uniform float sum = reduce_add(partial_sum) ;
  uniform int64 count = reduce_add(partial_count) ;

  if(count > 0)
    {
      if(average)
	      return sum / (float) count ;
      else
	      return sum * cdelt3 ;
    }
  else
    return 0.0f ;
} ;

export uniform float calculate_square_spectrumF16(uniform int16 cubeData[], uniform unsigned int stride, uniform int x1, uniform int x2, uniform int y1, uniform int y2, uniform bool average, uniform float cdelt3)
{
  float partial_sum = 0.0f ;
  unsigned int64 partial_count = 0 ;
  
  foreach (y=y1 ... y2, x=x1 ... x2)
    {      
      float tmp = half_to_float_fast(cubeData[y*stride+x]) ;
      bool nan = isnan(tmp) ;
      
      partial_sum += nan ? 0.0f : tmp ;
      partial_count += nan ? 0 : 1 ;
    } ;

  uniform float sum = reduce_add(partial_sum) ;
  uniform int64 count = reduce_add(partial_count) ;

  if(count > 0)
    {
      if(average)
	      return sum / (float) count ;
      else
	      return sum * cdelt3 ;
    }
  else
    return 0.0f ;
} ;

export uniform float calculate_radial_viewport_spectrumF16(uniform int16 cubeData[], uniform unsigned int cubeStride, uniform float pixels[], uniform bool mask[], uniform unsigned int stride, uniform int x1, uniform int x2, uniform int y1, uniform int y2, uniform int cx, uniform int cy, uniform int r2, uniform bool average, uniform float cdelt3)
{  
  float partial_sum = 0.0f ;
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

  uniform float sum = reduce_add(partial_sum) ;
  uniform int64 count = reduce_add(partial_count) ;

  if(count > 0)
    {
      if(average)
	      return sum / (float) count ;
      else
	      return sum * cdelt3 ;
    }
  else
    return 0.0f ;
} ;

export uniform float calculate_square_viewport_spectrumF16(uniform int16 cubeData[], uniform unsigned int cubeStride, uniform float pixels[], uniform bool mask[], uniform unsigned int stride, uniform int x1, uniform int x2, uniform int y1, uniform int y2, uniform bool average, uniform float cdelt3)
{
  float partial_sum = 0.0f ;
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

  uniform float sum = reduce_add(partial_sum) ;
  uniform int64 count = reduce_add(partial_count) ;

  if(count > 0)
    {
      if(average)
	      return sum / (float) count ;
      else
	      return sum * cdelt3 ;
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

export uniform float viewport_spectrum_rect(uniform struct fixed_block_t compressed[], uniform int width, uniform int height, uniform int x1, uniform int x2, uniform int y1, uniform int y2, uniform bool average, uniform float cdelt3)
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

    varying float partial_sum = 0.0f ;
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

        // TO-DO: fixed-point decompression (the moment of truth)      
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
            partial_sum += scale * (float)compressed[src_idx].mantissa[k];
            partial_count += isnan ? 0 : 1;
          }      
        }
      }

    uniform float sum = reduce_add(partial_sum) ;
    uniform int64 count = reduce_add(partial_count) ;

    if(count > 0)
    {
        if(average)
	        return sum / (float) count ;
        else
	        return sum * cdelt3 ;
    }
    else
        return 0.0f ;
}

export uniform float viewport_spectrum_circle(uniform struct fixed_block_t compressed[], uniform int width, uniform int height, uniform int x1, uniform int x2, uniform int y1, uniform int y2, uniform float cx, uniform float cy, uniform float r2, uniform bool average, uniform float cdelt3)
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

    varying float partial_sum = 0.0f ;
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

        // TO-DO: fixed-point decompression (the moment of truth)      
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
            partial_sum += scale * (float)compressed[src_idx].mantissa[k];
            partial_count += isnan ? 0 : 1;
          }      
        }
      }

    uniform float sum = reduce_add(partial_sum) ;
    uniform int64 count = reduce_add(partial_count) ;

    if(count > 0)
    {
        if(average)
	        return sum / (float) count ;
        else
	        return sum * cdelt3 ;
    }
    else
        return 0.0f ;
}

export uniform float viewport_image_spectrum_rect(uniform struct fixed_block_t compressed[], uniform int width, uniform int height, uniform float pixels[], uniform unsigned int8 mask[], uniform int stride, uniform int x1, uniform int x2, uniform int y1, uniform int y2, uniform bool average, uniform float cdelt3)
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

    varying float partial_sum = 0.0f ;
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

        // TO-DO: fixed-point decompression (the moment of truth)      
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

          varying int offset_x = pixel_x - x1;
          varying int offset_y = pixel_y - y1;
          varying int offset = offset_y*stride + offset_x;

          // check if a pixel is not a NaN and if it lies within the viewport window
          varying bool valid = !isnan && (pixel_x >= x1) && (pixel_x <= x2) && (pixel_y >= y1) && (pixel_y <= y2);          

          cif(valid)
          {                        
            partial_sum += scale * (float)compressed[src_idx].mantissa[k];
            partial_count += isnan ? 0 : 1;
          }      
        }
      }

    uniform float sum = reduce_add(partial_sum) ;
    uniform int64 count = reduce_add(partial_count) ;

    if(count > 0)
    {
        if(average)
	        return sum / (float) count ;
        else
	        return sum * cdelt3 ;
    }
    else
        return 0.0f ;
}