#' @export
include_script <- function(src, language="", caption=""){
  if (language != "") langstring = sprintf(".%s", language) else lanstring = ""
  if (caption != "") capstring = sprintf('htmlcap="%s"', caption) else capstring = ""

  intro = sprintf("```{%s %s}", langstring, capstring)

  knitr::asis_output(paste(c(intro,
                             readLines(src),
                             '```'), collapse="\n"))
}
#' @export
pdf_report <- function(...){
  if(!require(printr)){
    warning("Automatic table creation not enabled. See https://github.com/yihui/printr for install info.")
  }
  rmarkdown::pdf_document(...)
}

word_report <- function(...){
  if(!require(printr)){
    warning("Automatic table creation not enabled. See https://github.com/yihui/printr for install info.")
  }
  rmarkdown::word_document(...)
}

#' @export
html_report <- function(highlight= TRUE, mathjax = "default", fig_width = 6.5, fig_height=4, fig_device='svg', ...) {

  panArgs = c()

  if(!require(printr)){
    warning("Automatic table creation not enabled. See https://github.com/yihui/printr for install info.")
  }

  if(highlight) {
    theme <- system.file("rmarkdown","templates","html_report","skeleton", "prism.css",
                         package="Reports")

    highlightJs <- system.file("rmarkdown","templates","html_report","skeleton", "prism.js",
                               package="Reports")

    highlightR <- system.file("rmarkdown","templates","html_report","skeleton", "prism.r.js",
                               package="Reports")

    cssPath <- rmarkdown::render_supporting_files(theme, "Report_asset_files")
    jsPath <- rmarkdown::render_supporting_files(highlightJs, "Report_asset_files")
    rjsPath <- rmarkdown::render_supporting_files(highlightR, "Report_asset_files")

    panArgs <- c(panArgs,
                 "--variable", paste("mmhighlight-css=",cssPath,sep=""),
                 "--variable", paste("mmhighlight-js=",jsPath,sep=""),
                 "--variable", paste("mmhighlight-rjs=",rjsPath,sep=""))

    knitr::opts_chunk$set(label=".language-r")
  }

  if(mathjax == "default"){
    panArgs <- c(panArgs,
                 "--variable", "mathjax=http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML")
  } else if(!is.null(mathjax) && mathjax != ""){
    panArgs <- c(panArgs,
                 "--variable", paste("mathjax=",mathjax,sep=""))
  }

  template_path <- system.file("rmarkdown","templates","html_report","template.html",
                               package="Reports")

  knitr::opts_chunk$set(dev="svg")

  knitr::knit_hooks$set(htmlcap = function(before, options, envir) {
    if(options$htmlcap){
      if(before) {
        paste("<figure>")
      }
      else {
        paste('<figcaption>',options$fig.cap,"</figcaption></figure>",sep="")
      }
    }
  })

  # call the base html_document function
  rmarkdown::html_document(highlight= NULL,
                        template = template_path,
                        fig_width = fig_width,
                        fig_device = fig_device,
                        pandoc_args = panArgs,
                        fig_height = fig_height, ...)
}
#' @export
add_image <- function(default, alt="", sameLine=TRUE, ...){
  if(interactive())
    toFormat <- "interactive"
  else
    toFormat <- knitr::opts_knit$get("rmarkdown.pandoc.to")

  overrides <- list(...)

  if (length(overrides) > 0 && exists(toFormat,overrides)){
    file <- overrides[toFormat]
  }  else {
    file <- default
  }
  if(!sameLine) nl <- "\n\n" else nl <- ""
  knitr::asis_output(sprintf("![%s](%s)%s", alt, file, nl))
}

html_document <- function(toc = FALSE,
                          toc_depth = 3,
                          number_sections = FALSE,
                          fig_width = 7,
                          fig_height = 5,
                          fig_retina = if (!fig_caption) 2,
                          fig_caption = FALSE,
                          smart = TRUE,
                          self_contained = TRUE,
                          theme = "default",
                          highlight = "default",
                          mathjax = "default",
                          template = "default",
                          css = NULL,
                          includes = NULL,
                          keep_md = FALSE,
                          lib_dir = NULL,
                          pandoc_args = NULL,
                          ...) {

  library(rmarkdown)
  # build pandoc args
  args <- c("--standalone")
  # use section divs
  args <- c(args, "--section-divs")
  # table of contents
  args <- c(args, pandoc_toc_args(toc, toc_depth))
  # template path and assets
  if (identical(template, "default"))
    args <- c(args, "--template",
              pandoc_path_arg(rmarkdown_system_file("rmd/h/default.html")))
  else if (!is.null(template))
    args <- c(args, "--template", pandoc_path_arg(template))
  # numbered sections
  if (number_sections)
    args <- c(args, "--number-sections")
  # additional css
  for (css_file in css)
    args <- c(args, "--css", pandoc_path_arg(css_file))
  # pre-processor for arguments that may depend on the name of the
  # the input file (e.g. ones that need to copy supporting files)
  pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir,
                            output_dir) {
    # use files_dir as lib_dir if not explicitly specified
    if (is.null(lib_dir))
      lib_dir <- files_dir
    # extra args
    args <- c()
    # content includes (we do this here so that user include-in-header content
    # goes after dependency generated content). make the paths absolute if
    # making a Shiny document so we can resolve them even if rendering
    # elsewhere.
    args <- c(args, includes_to_pandoc_args(includes,
                                            filter = if (identical(runtime, "shiny"))
                                              normalize_path
                                            else
                                              identity))
    # return additional args
    args
  }
  # return format
  output_format(
    knitr = knitr_options_html(fig_width, fig_height, fig_retina, keep_md),
    pandoc = pandoc_options(to = "html",
                            from = from_rmarkdown(fig_caption),
                            args = args),
    keep_md = keep_md,
    clean_supporting = self_contained,
    pre_processor = pre_processor,
    base_format = html_document_base(smart = smart, theme = theme,
                                     self_contained = self_contained,
                                     lib_dir = lib_dir, mathjax = mathjax,
                                     template = template,
                                     pandoc_args = pandoc_args, ...)
  )
}

knitr_options_html <- function(fig_width, fig_height, fig_retina, keep_md) {
  opts_chunk <- list(dev = 'svg',
                     dpi = 96,
                     fig.width = fig_width,
                     fig.height = fig_height,
                     fig.retina = fig_retina)
  if (keep_md)
    opts_chunk$fig.retina <- NULL
  knitr_options(opts_chunk = opts_chunk)
}

pandoc_output_file <- function(input, pandoc_options) {
  to <- pandoc_options$to
  if (!is.null(pandoc_options$ext))
    ext <- pandoc_options$ext
  else if (to %in% c("latex", "beamer"))
    ext <- ".pdf"
  else if (to %in% c("html", "html5", "s5", "slidy",
                     "slideous", "dzslides", "revealjs"))
    ext <- ".html"
  else if (grepl("^markdown", to)) {
    if (!identical(tolower(tools::file_ext(input)), "md"))
      ext <- ".md"
    else {
      ext <- paste(".", strsplit(to, "[\\+\\-]")[[1]][[1]], sep = "")
    }
  }
  else
    ext <- paste(".", to, sep = "")
  output <- paste(tools::file_path_sans_ext(input), ext, sep = "")
  basename(output)
}
rmarkdown_system_file <- function(file) {
  system.file(file, package = "rmarkdown")
}
from_rmarkdown <- function(implicit_figures = TRUE) {
  rmarkdown_format(ifelse(implicit_figures, "", "-implicit_figures"))
}