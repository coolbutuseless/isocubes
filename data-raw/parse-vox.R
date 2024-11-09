library(grid)
library(ctypesio)
library(isocubes)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Need to reverse bytes and convert to just R hex strings
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hexpal <- c(
  "00000000", "ffffffff", "ffccffff", "ff99ffff", "ff66ffff", "ff33ffff", "ff00ffff", "ffffccff", "ffccccff", "ff99ccff", "ff66ccff", "ff33ccff", "ff00ccff", "ffff99ff", "ffcc99ff", "ff9999ff",
  "ff6699ff", "ff3399ff", "ff0099ff", "ffff66ff", "ffcc66ff", "ff9966ff", "ff6666ff", "ff3366ff", "ff0066ff", "ffff33ff", "ffcc33ff", "ff9933ff", "ff6633ff", "ff3333ff", "ff0033ff", "ffff00ff",
  "ffcc00ff", "ff9900ff", "ff6600ff", "ff3300ff", "ff0000ff", "ffffffcc", "ffccffcc", "ff99ffcc", "ff66ffcc", "ff33ffcc", "ff00ffcc", "ffffcccc", "ffcccccc", "ff99cccc", "ff66cccc", "ff33cccc",
  "ff00cccc", "ffff99cc", "ffcc99cc", "ff9999cc", "ff6699cc", "ff3399cc", "ff0099cc", "ffff66cc", "ffcc66cc", "ff9966cc", "ff6666cc", "ff3366cc", "ff0066cc", "ffff33cc", "ffcc33cc", "ff9933cc",
  "ff6633cc", "ff3333cc", "ff0033cc", "ffff00cc", "ffcc00cc", "ff9900cc", "ff6600cc", "ff3300cc", "ff0000cc", "ffffff99", "ffccff99", "ff99ff99", "ff66ff99", "ff33ff99", "ff00ff99", "ffffcc99",
  "ffcccc99", "ff99cc99", "ff66cc99", "ff33cc99", "ff00cc99", "ffff9999", "ffcc9999", "ff999999", "ff669999", "ff339999", "ff009999", "ffff6699", "ffcc6699", "ff996699", "ff666699", "ff336699",
  "ff006699", "ffff3399", "ffcc3399", "ff993399", "ff663399", "ff333399", "ff003399", "ffff0099", "ffcc0099", "ff990099", "ff660099", "ff330099", "ff000099", "ffffff66", "ffccff66", "ff99ff66",
  "ff66ff66", "ff33ff66", "ff00ff66", "ffffcc66", "ffcccc66", "ff99cc66", "ff66cc66", "ff33cc66", "ff00cc66", "ffff9966", "ffcc9966", "ff999966", "ff669966", "ff339966", "ff009966", "ffff6666",
  "ffcc6666", "ff996666", "ff666666", "ff336666", "ff006666", "ffff3366", "ffcc3366", "ff993366", "ff663366", "ff333366", "ff003366", "ffff0066", "ffcc0066", "ff990066", "ff660066", "ff330066",
  "ff000066", "ffffff33", "ffccff33", "ff99ff33", "ff66ff33", "ff33ff33", "ff00ff33", "ffffcc33", "ffcccc33", "ff99cc33", "ff66cc33", "ff33cc33", "ff00cc33", "ffff9933", "ffcc9933", "ff999933",
  "ff669933", "ff339933", "ff009933", "ffff6633", "ffcc6633", "ff996633", "ff666633", "ff336633", "ff006633", "ffff3333", "ffcc3333", "ff993333", "ff663333", "ff333333", "ff003333", "ffff0033",
  "ffcc0033", "ff990033", "ff660033", "ff330033", "ff000033", "ffffff00", "ffccff00", "ff99ff00", "ff66ff00", "ff33ff00", "ff00ff00", "ffffcc00", "ffcccc00", "ff99cc00", "ff66cc00", "ff33cc00",
  "ff00cc00", "ffff9900", "ffcc9900", "ff999900", "ff669900", "ff339900", "ff009900", "ffff6600", "ffcc6600", "ff996600", "ff666600", "ff336600", "ff006600", "ffff3300", "ffcc3300", "ff993300",
  "ff663300", "ff333300", "ff003300", "ffff0000", "ffcc0000", "ff990000", "ff660000", "ff330000", "ff0000ee", "ff0000dd", "ff0000bb", "ff0000aa", "ff000088", "ff000077", "ff000055", "ff000044",
  "ff000022", "ff000011", "ff00ee00", "ff00dd00", "ff00bb00", "ff00aa00", "ff008800", "ff007700", "ff005500", "ff004400", "ff002200", "ff001100", "ffee0000", "ffdd0000", "ffbb0000", "ffaa0000",
  "ff880000", "ff770000", "ff550000", "ff440000", "ff220000", "ff110000", "ffeeeeee", "ffdddddd", "ffbbbbbb", "ffaaaaaa", "ff888888", "ff777777", "ff555555", "ff444444", "ff222222", "ff111111"
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Palette decoding from Little-endian hex string to #RRGGBBAA
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hexint_to_hexcol <- function(hex) {
  alpha <- substr(hex, 1, 2)
  blue  <- substr(hex, 3, 4)
  green <- substr(hex, 5, 6)
  red   <- substr(hex, 7, 8)
  
  paste0('#', red, green, blue, alpha)
}

pal <- hexint_to_hexcol(hexpal)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Chunk header is always the content size (excluding the header) and the children size
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_chunk_header <- function(con) {
  list(
    nbytes_content  = read_int32(con), # content
    nbytes_children = read_int32(con) # children
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_main <- function(con) {
  header <- parse_chunk_header(con)
  
  list(
    main = header
  )
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_rgba <- function(con) {
  
  header <- parse_chunk_header(con)
  
  pal <- read_uint32(con, 256, promote = 'hex') 
  
  list(
    rgba = c(header, list(pal = pal))
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Parse a material
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_matt <- function(con) {
  header <- parse_chunk_header(con)
  
  # print(header)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Read raw bytes for matt. investgate later
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  bytes <- read_raw(con, header$nbytes_content)
  
  list(matt = c(
    header,
    list(bytes = bytes)
  )
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_size <- function(con) {
  header <- parse_chunk_header(con)
  
  res <- list()
  res$x <- read_int32(con) # x
  res$y <- read_int32(con) # y
  res$z <- read_int32(con) # z
  
  # SIZE chunk is always followed by a XYZI chunk
  chunk_id <- read_str_raw(con, 4)
  if (chunk_id != 'XYZI') {
    print(chunk_id)
    print(seek(con))
    stop("Expected 'XYZI' chunk")
  }
  
  read_int32(con) # content
  read_int32(con) # children
  
  res$nvoxels <- read_int32(con)
  cat("Nvoxels: ", res$nvoxels, "\n")
  
  dat <- read_uint8(con, 4 * res$nvoxels)
  res$coords <- matrix(dat, ncol = 4, byrow = TRUE) |> 
    as.data.frame() |>
    setNames(c('x', 'y', 'z', 'color_idx')) 
  
  list(size = c(
    header,
    res
  ))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Parse PACK chunk
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_pack <- function(con) {
  header <- parse_chunk_header(con)
  
  res <- list()
  nchunks <- res$nchunks <- read_int32(con)
  
  message("PACK: nchunks = ", nchunks)
  
  models <- lapply(seq_len(nchunks), function(i) {
    chunk_id <- read_str_raw(con, 4)
    if (chunk_id == "") break; # EOF
    print(chunk_id)
    
    switch(
      chunk_id,
      SIZE = parse_size(con),
      stop("parse_pak(): Unknown chunk_id: ", chunk_id)
    )
  })
  
  list(
    pack = c(
      header,
      list(models = models)
    )
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_chunks <- function(con) {
  
  chunk_data <- list()
  while (TRUE) {
    chunk_id <- read_str_raw(con, 4)
    if (chunk_id == "") break; # EOF
    print(chunk_id)
    
    res <- switch(
      chunk_id,
      MAIN = parse_main(con),
      SIZE = parse_size(con),
      RGBA = parse_rgba(con),
      PACK = parse_pack(con),
      MATT = parse_matt(con),
      stop("Unknown chunk_id: ", chunk_id)
    )
    
    # res[[1]]$nybtes_content  <- nbytes_content
    # res[[1]]$nbytes_children <- nbytes_children
    
    chunk_data <- c(chunk_data, res)
  }
  
  chunk_data
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_vox <- function(filename) {
  con <- file(filename, 'rb')
  con <- set_eof_check(con, 'ignore')
  on.exit(close(con))
  
  magic <- read_str_raw(con, 4) 
  stopifnot(magic == 'VOX ')
  
  vox <- list(file = filename)
  vox$version <- read_int32(con)
  
  chunkdata <- parse_chunks(con)
  vox <- c(vox, chunkdata)
  
  vox
}



# filename <- "./data-raw/vox/beagle.vox"
filename <- "./data-raw/vox/menger.vox"
# filename <- "./data-raw/vox/monument/monu4.vox"
# filename <- "./data-raw/vox/anim/deer.vox"
# readBin(vfile, 'raw', file.size(vfile))

vox <- parse_vox(filename)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Normalize coordinates to centre on (0, 0, 0)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# coords$x0 <- coords$x - mean(coords$x)
# coords$y0 <- coords$z - mean(coords$y)
# coords$z0 <- coords$y - mean(coords$y)
# 
# coords$x <- coords$x0
# coords$y <- coords$z0
# coords$z <- coords$y0

if (FALSE) {
  grobs <- lapply(vox$models, function(mod) {
    isocubesGrob(mod$coords, x = 0.25, y = 0.25, size = 4, xyplane = 'flat')
  })
  
  for (i in 1:32) {
    grid.rect(gp = gpar(fill = 'white'))
    grid.draw(grobs[[(i %% 4) + 1]])
    Sys.sleep(0.2)
  }
}



if (TRUE) {
  coords <- vox$size$coords %||% vox$pack$models[[1]]$size$coords
  cubes <- isocubesGrob(
    coords, 
    x = 0.5, y = 0, 
    size = 1, 
    col = NA, 
    xyplane = 'right'
  )
  tmp <- dev.hold(); grid.newpage(); grid.draw(cubes); tmp <- dev.flush()
}







