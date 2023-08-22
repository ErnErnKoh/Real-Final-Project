# EPI 590R R Script

library(readxl)
library(tidyverse)
library(gtsummary)
titanic_data <- read_xlsx(here::here("data", "titanic 2023.xlsx"))|>
  mutate(Survived_cat = factor(Survived, labels = c("(0) No", "(1) Yes")),
         TicketClass_cat = factor(TicketClass, labels = c("First", "Second", "Third")))

# Descriptive Statistics

table1 <- tbl_summary(
  titanic_data,
  by = Sex,
  include = c(Age, TicketClass, Survived),
  label = list(
    Age ~ "Age in Years",
    TicketClass ~ "Ticket Class",
    Survived ~ "Survived"
  ),
  missing_text = "Missing") |>
  add_p(test = list(all_continuous() ~ "t.test", 
                    all_categorical() ~ "chisq.test")) |> 
  add_overall(col_label = "**Total**") |> 
  bold_labels() |> 
  modify_footnote(update = everything() ~ NA) |> 
  modify_header(label = "**Variable**", p.value = "**P**")
table1

survived <- list(number_survival = sum(titanic_data$Survived == 1, na.rm = TRUE))
died <- list(number_died = sum(titanic_data$Survived == 0, na.rm = TRUE))

survived_f <- inline_text(table1, variable = "Survived", column = "female")
survived_m <- inline_text(table1, variable = "Survived", column = "male")

# Univariate Logistic Regression

table2 <- tbl_uvregression(
  titanic_data, 
  y = Survived_cat,
  include = c(Age, TicketClass_cat, Sex),
  method = glm,
  method.args = list(family = binomial()),
  exponentiate = TRUE,
  label = list(Age ~ "Age",
               Sex ~ "Sex",
               TicketClass_cat ~ "Ticket Class"
  ))
table2

uniOR_report <- inline_text(table2, variable = "TicketClass_cat", level = "Second")

# Multivariate Logistic Regression

logistic_model <- glm(Survived_cat ~ Age + Sex + TicketClass_cat, 
                      data = titanic_data, family = binomial())
or_table <- tbl_regression(
  logistic_model,
  exponentiate = TRUE,
  label = list(
    Age ~ "Age",
    Sex ~ "Sex",
    TicketClass_cat ~ "Ticket Class"
  ))
or_table
or_report <- inline_text(or_table, variable="Sex", level = "male")

# OR Estimate Plot

library(broom)
summary(logistic_model)
glance(logistic_model)

options(digits = 3)
summary(logistic_model)$coefficients

tidy(logistic_model, conf.int = TRUE, exponentiate = TRUE) |>
  tidycat::tidy_categorical(logistic_model, exponentiate = TRUE) |>
  dplyr::slice(-1) |> 
  ggplot(mapping = aes(x = level, y = estimate,
                       ymin = conf.low, ymax = conf.high)) +
  geom_point() +
  geom_errorbar() +
  facet_grid(cols = vars(variable), scales = "free", space = "free") +
  scale_y_log10()

ggsave(filename = here::here("data", "fig_one_forest_plot.pdf"))

# Functions

x <- 0.89
1/x 
reciprocalOR <- function(x) {
  1/x}
reciprocalOR(0.46)

#Proportion function
y <- c(0,1,1,0)
multiplier <- 100
multiplier * sum(y)/length(y)

prop <- function(y, multiplier){
  n <- length(y)
  prop_val <- multiplier * sum(y)/n
  return(prop_val)
}

prop_f <- prop(y = titanic_data$Sex == "female", multiplier = 100)
options(digits = 3)

emo::ji("dog") #just for fun