library(scales)


run_experiment = function(optimalg, eval, dims, x0, ...) {
  dims %>% purrr::map_dfr(function(dim) {
    result = optimalg(
      rep(x0, dim),
      fn = eval,
      lower = -100,
      upper = 100,
      ...
    )
    extract_diagnostic_data(result, eval) %>%
    dplyr::mutate(Dim = dim)
  })
}

run_massive_experiments = function(optim, evals, dims, x0, control) {
    expand.grid(
        eval = evals,
        dim = dims
    ) %>%
    purrr::pmap_dfr(function(eval, dim) {
        result = optim(rep(x0, dim), fn = eval, lower = -100, upper = 100, control = control)
        extract_diagnostic_data(result, eval) %>%
        dplyr::mutate(
            Dim = dim,
            Func = attr(eval, "name")
        )
    })
}

extract_diagnostic_data = function(expresult, eval) {
  iter = expresult %>% purrr::pluck("diagnostic", "bestVal") %>% length() 
  best_value_pop = expresult %>% get_best_value(eval)
  tibble::tibble(
    t = 1:iter,
    best_sofar = expresult %>% purrr::pluck("diagnostic", "bestVal") %>% as.numeric(),
    best_pop = best_value_pop,
    sigma = expresult %>% purrr::pluck("diagnostic", "Ft"),
    label = expresult %>% purrr::pluck("label") 
  )
}

get_best_value = function(dfx, eval) {
  population = dfx %>% purrr::pluck("diagnostic", "pop")
  c(dim_, lambda_, gen) %<-% dim(population)
  1:gen %>% purrr::map(function(g) {
    population[,,g] %>% 
      apply(2, eval) %>%
      min()
  }) %>%
  purrr::flatten_dbl()
}

get_mean_value = function(dfx, eval) {
  population = dfx %>% purrr::pluck("diagnostic", "pop")
  c(dim_, lambda_, gen) %<-% dim(population)
  1:gen %>% purrr::map(function(g) {
    population[,,g] %>% 
      apply(1, base::mean) %>%
      eval() %>%
      as.numeric()
  }) %>%
  purrr::flatten_dbl()
}


merge_results = function(dfxs) {
  dfxs %>%
    purrr::reduce(dplyr::bind_rows)
}

vg_plot = function(data, yval, color_column = "label", scale_y_log = FALSE, xmax = NULL) {
  colors = c(
   "#4E84C4",
    "#C4961A",
    "#944f86",
    "#293352",
    "#b3b5e8",
    "#ff0022",
    "#D16103",
    "#C3D7A4",
    "#52854C",
    "#000000"
  )
  base_plot = 
    data %>%
    ggplot2::ggplot(aes(x = t, y = !!rlang::sym(yval), color = !!rlang::sym(color_column))) +
    ggplot2::geom_line(linetype = "solid", size = 0.9) + 
    ggplot2::theme_light() + 
    ggplot2::theme(
      axis.title = element_text(size = 15, face = "bold"),
      axis.text = element_text(size = 15, face = "bold"),
      legend.text = element_text(size = 25, face = "bold"),
      legend.title = element_text(size = 25, face = "bold"),
      strip.text.x = element_text(size = 20, colour = "black"),
      strip.text.y = element_text(size = 20, colour = "red"),
      legend.key.width = unit(5,"cm")
    ) + 
    ggplot2::scale_color_manual(values = colors) + 
    xlab("t") +
    ylab(yval)
  if (scale_y_log) {
    base_plot = base_plot +
      scale_y_log10(
        breaks = scales::trans_breaks("log10", function(x) 10^x),
        labels = scales::trans_format("log10", scales::math_format(10^.x))
      )
  } 
  if (!is.null(xmax)) {
    base_plot + xlim(0, xmax)
  } else {
    base_plot
  }
}

vv_plot = function(data, xval, yval, scale_x_log = FALSE, scale_y_log = FALSE) {
  colors = c(
    "#FFDB6D",
    "#C4961A",
    "#944f86",
    "#eaafaf",
    "#b3b5e8",
    "#ff0022",
    "#D16103",
    "#C3D7A4",
    "#52854C",
    "#4E84C4",
    "#293352",
    "#000000"
  )

  base_plot = 
    data %>%
    ggplot2::ggplot(aes(x = !!rlang::sym(xval), y = !!rlang::sym(yval), col = label)) +
    ggplot2::geom_point() + 
    ggplot2::geom_line(linetype = "dashed") + 
    ggplot2::theme_light() + 
    ggplot2::theme(
      axis.title = element_text(size = 15, face = "bold"),
      axis.text = element_text(size = 15, face = "bold"),
      legend.text = element_text(size = 15, face = "bold"),
      legend.title = element_text(size = 15, face = "bold"),
    ) +
    ggplot2::scale_color_manual(values = colors) + 
    xlab(xval) + 
    ylab(yval)
  if (scale_y_log) {
    base_plot = base_plot +
      scale_y_log10(
        breaks = scales::trans_breaks("log10", function(x) 10^x),
        labels = scales::trans_format("log10", scales::math_format(10^.x))
      )
  } 
  if (scale_x_log) {
    base_plot = base_plot + 
      scale_x_log10(
        breaks = scales::trans_breaks("log10", function(x) 10^x),
        labels = scales::trans_format("log10", scales::math_format(10^.x))
      )
  }
  base_plot
}
