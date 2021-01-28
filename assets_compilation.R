# Script to compile the CSS and JavaScript for deployment

# Load the js_parser function
source('./utils/helper_functions.R')

# Remove old files
file.remove(list.files('./www', pattern = 'main.css|metalpdataportal.js', full.names = TRUE))

# Compile CSS from Sass
sass::sass(
  sass::sass_file('assets/sass/main.scss'), 
  output = paste0('www/', gsub('[- :]', '', Sys.time()), '-main.css'),
  options = sass::sass_options(output_style = 'compressed')
)

# Compile and minify JavaScript
js_parser(env = 'prod')
