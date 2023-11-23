#Chapitre 11 Programmer avec dplyr



is_num2 <- function(x){
  is.numeric(x) &&
    !isTRUE(all.equal(x, as.integer(x)))
}

half_value <- function(z) z / 2

# talk about the .keep option
mtcars %>%
  mutate(across(where(is_num2), half_value), .keep = "none") %>%
  rownames_to_column()



mtcars %>%
  group_by(cyl) %>%
  summarise(
    across(
      .cols = where(is_num2),
      .fns = list(
        mean = mean,
        sd = sd,
        median = median
      )
    )
  ) %>%
  tibble::rownames_to_column()


mtcars %>% group_by(cyl) %>%
  summarise(across(
    .cols = where(is_num2),
    .fns = list(mean = mean, sd = sd, median = median),
    .names = "{.fn}_{.col}"
  )) %>%
  rownames_to_column()

mtcars

dat <- nest(mtcars, subset = !cyl)
dat
