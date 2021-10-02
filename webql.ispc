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