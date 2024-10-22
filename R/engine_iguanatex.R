#' Provides iguanatex functionality for knitr
#' 
#' @param options List.  Standard input of a knitr engine.
#' @details Mimics the IguanaTex tool for PowerPoint.  Requires \code{pdflatex} and \code{dvisgm}.
#' 
#' Compiles Latex code into and svg image and inserts it to the document.
#' @md
#' @keywords knitr
#' @export
engine_iguanatex = function(options) {
  if (!options$eval) return(engine_output(options, options$code, ''))
  s1 = paste0(options$code,collapse="\n")
  xfun::write_utf8(s1, texf <- knitr:::wd_tempfile('tikz', '.tex'))
  on.exit(unlink(texf), add = TRUE)
  
  out1 = system2('pdflatex',c('-output-format dvi -shell-escape -interaction=batchmode', texf))
  if(out1) stop("tex file did not compile")
  on.exit(unlink(xfun:::with_ext(texf, "aux")),add=TRUE)
  on.exit(unlink(xfun:::with_ext(texf, "log")),add=TRUE)
  outf <- xfun:::with_ext(texf, "dvi")

  fig = knitr:::fig_path('.dvi', options)
  dir.create(dirname(fig), recursive = TRUE, showWarnings = FALSE)
  file.rename(outf, fig)

  fig2 = xfun:::with_ext(fig, "svg")
  out2 <- system2('dvisvgm', c('--no-fonts', '-o', shQuote(fig2), fig))
  if(out2) stop("conversion from dvi to svg failed")
  fig = fig2

  options$fig.num = 1L; options$fig.cur = 1L
  extra = knitr:::run_hook_plot(fig, options)
  knitr:::engine_output(options, options$code, '', extra)
}
