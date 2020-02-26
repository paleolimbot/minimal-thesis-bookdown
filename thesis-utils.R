
library(tidyverse)

# it's unlikely you'll be able to make the tables you want 
# without the kableExtra package
library(kableExtra)

# you probably want all these chunk options to be the same
# for every chapter
# chunk options
knitr::opts_chunk$set(
  echo = FALSE,
  fig.align = "center"
)

# word documents have very low figure resolution by default, which is
# unlikely to impress your supervisor
is_word_output <- function() {
  !knitr::is_html_output() && !knitr::is_latex_output()
}

if (is_word_output()) {
  knitr::opts_chunk$set(dpi = 300)
}

# default theme function...useful to be able to use this
# theme with different base font sizes in case you run into
# trouble with this later on
theme_thesis <- function(...) {
  theme_bw(...) + theme(strip.background = element_blank())
}

# set the default ggplot2 theme
theme_set(theme_thesis(10))

# I think a custom "thesis_kable" function is useful
# for consistent tables throughout the thesis. It's a wrapper
# around kable(), and you can add the kableExtra modifiers
# after it to further modify the output

thesis_kable <- function(tbl, ..., style = list(), longtable = FALSE) {
  # A font size of 10pt in tables is likely to save you at least a little
  # trouble
  default_style <- list(font_size = if (knitr::is_latex_output()) 10)
  style_options <- c(style, default_style)[union(names(style), names(default_style))]
  
  # kableExtra doesn't do raw markdown output
  if (!is_word_output()) {
    kbl <- kable(
      tbl,
      ...,
      booktabs = TRUE,
      longtable = longtable
    )
    
    rlang::exec(kableExtra::kable_styling, kbl, !!!style_options)
  } else {
    knitr::kable(tbl, format = "pandoc", ...)
  }
}

# this lets you use some markdown markup for table cell values
thesis_kable_raw_markdown <- function(tbl, ...) {
  if (knitr::is_latex_output()) {
    tbl <- tbl_markdown_to_latex(tbl)
  }
  
  thesis_kable(
    tbl, 
    escape = FALSE,
    ...
  )
}

# this lets you use some markdown markup for figure captions and short captions
md_caption <- function(x) {
  if (knitr::is_latex_output()) {
    markdown_to_latex(x)
  } else {
    x
  }
}

markdown_to_latex <- function(x) {
  x %>% 
    str_replace_all("\\*\\*(.*?)\\*\\*", "\\\\textbf{\\1}") %>% 
    str_replace_all("\\*(.*?)\\*", "\\\\emph{\\1}") %>% 
    str_replace_all("\\^(.*?)\\^", "$^{\\\\text{\\1}}$") %>% 
    str_replace_all("~(.*?)~", "$_{\\\\text{\\1}}$") %>% 
    str_replace_all("%", "\\\\%")
}

tbl_markdown_to_latex <- function(tbl) {
  tbl %>% 
    mutate_if(is.character, markdown_to_latex) %>% 
    mutate_if(is.factor, markdown_to_latex) %>% 
    rename_all(markdown_to_latex)
}
