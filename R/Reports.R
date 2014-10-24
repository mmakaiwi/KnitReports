
include_script <- function(src, language="", caption=""){
  if (language != "") langstring = sprintf(".%s", language) else lanstring = ""
  if (caption != "") capstring = sprintf('htmlcap="%s"', caption) else capstring = ""

  intro = sprintf("```{%s %s}", langstring, capstring)

  knitr::asis_output(paste(c(intro,
                             readLines(src),
                             '```'), collapse="\n"))
}

html_report <- function(highlight= TRUE, ...) {

  panArgs = NULL

  library(printr)

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

    panArgs <- c("--variable", paste("mmhighlight-css=",cssPath,sep=""),
                 "--variable", paste("mmhighlight-js=",jsPath,sep=""),
                 "--variable", paste("mmhighlight-rjs=",rjsPath,sep=""))

    knitr::opts_chunk$set(label=".language-r")
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
                           fig_width = 6.5,
                           pandoc_args = panArgs,
                           fig_height = 4, ...)
}

addImage <- function(default, alt="", sameLine=TRUE, ...){
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