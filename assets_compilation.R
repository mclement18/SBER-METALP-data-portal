# Script to compile the CSS and JavaScript for deployment

# Load the js_parser function
source('./utils/helper_functions.R')

# Remove old files
file.remove(list.files('./www', pattern = 'main.css|metalpdataportal.js', full.names = TRUE))

# Compile CSS from Sass
cssfile <- paste0(gsub('[- :]', '', Sys.time()), '-main.css')
sass::sass(
  sass::sass_file('assets/sass/main.scss'), 
  output = paste0('www/', cssfile),
  options = sass::sass_options(output_style = 'compressed')
)

# Compile and minify JavaScript
jsfile <- js_parser(env = 'prod')

# Create file with assets name
readr::write_file(paste(
  paste0('STYLESHEET_FILE <- "', cssfile, '"'),
  paste0('JAVASCRIPT_FILE <- "', jsfile, '"'),
  sep = '\n'
), file = './assets_name.R')
