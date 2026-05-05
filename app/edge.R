library(magick)

# parameters for canny edge detection — "radiusXsigma+lower%+upper%"
# sigma:  blur before edge detection
# lower%: lower echelon of edge strength
# upper%: upper echelon of edge strength
CANNY_GEOMETRY <- '0x2+5%+15%'

# the pink pixel mask will have a border to capture nearby edges 
# pink pixels because the parts of the goat we'd focus on are pink 
PINK_DILATION <- 29

img_color <- image_read('images/RUA_TD.jpeg') %>%
  image_scale('60%')

# Binary mask: 1 (white) where pink, 0 (black) elsewhere.
# u.r/u.g/u.b are the pixel's R/G/B channels normalised to 0 - 1.
# Pink udder tissue has red clearly above both green and blue;
# white fur and gray background both fail the difference conditions.
pink_mask <- image_fx(img_color,
  expression = "u.r>0.55 && u.r-u.g>0.08 && u.r-u.b>0.06 ? 1 : 0") %>%
  image_convert(colorspace = 'Gray')

# expand the boundary 
pink_mask_dilated <- image_morphology(pink_mask,
  method = 'Dilate',
  kernel = paste0('Disk:', PINK_DILATION))

# mask invert, since masked edges are usually white, but we want it the other way around
pink_mask_inv <- image_negate(pink_mask_dilated)

# run canny edge detection and grayscale images
canny <- img_color %>%
  image_convert(colorspace = 'Gray') %>%
  image_canny(geometry = CANNY_GEOMETRY)

# invert the canny
result <- image_negate(canny)

image_write(result, 'images/edge.jpg')
print(result)
