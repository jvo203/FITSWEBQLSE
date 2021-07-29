export uniform float calculate_radial_spectrumF16(uniform int16_t cubeData[], uniform unsigned int width, uniform int x1, uniform int x2, uniform int y1, uniform int y2, uniform int cx, uniform int cy, uniform int r2, uniform bool average, uniform float cdelt3)
{  
  float partial_sum = 0.0f ;
  unsigned int64_t partial_count = 0 ;
  
  foreach (y=y1 ... y2, x=x1 ... x2)
    {      
      float tmp = half_to_float_fast(cubeData[y*width+x]) ;
      bool nan = isnan(tmp)

      float dist2 = (cx-x)*(cx-x) + (cy-y)*(cy-y) ;
      bool exclude = (dist2 > r2) ;
      
      partial_sum += (nan || exclude) ? 0.0f : tmp ;
      partial_count += (nan || exclude) ? 0 : 1 ;
    } ;

  uniform float sum = reduce_add(partial_sum) ;
  uniform int64_t count = reduce_add(partial_count) ;

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

export uniform float calculate_square_spectrumF16(uniform int16_t cubeData[], uniform unsigned int width, uniform int x1, uniform int x2, uniform int y1, uniform int y2, uniform bool average, uniform float cdelt3)
{
  float partial_sum = 0.0f ;
  unsigned int64_t partial_count = 0 ;
  
  foreach (y=y1 ... y2, x=x1 ... x2)
    {      
      float tmp = half_to_float_fast(cubeData[y*width+x]) ;
      bool nan = isnan(tmp)
      
      partial_sum += nan ? 0.0f : tmp ;
      partial_count += nan ? 0 : 1 ;
    } ;

  uniform float sum = reduce_add(partial_sum) ;
  uniform int64_t count = reduce_add(partial_count) ;

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