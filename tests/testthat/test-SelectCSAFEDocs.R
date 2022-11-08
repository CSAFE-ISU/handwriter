test_that("an error occurs when the sum of template and model writers is greater than the total number of CSAFE writers", {
  expect_error(select_csafe_docs(num_template_writers = 100,
                                 template_sessions = 1,
                                 template_reps = 1,
                                 template_prompts = "London Letter",
                                 template_seed = 100,
                                 num_model_writers = 200,
                                 model_sessions = 1,
                                 model_reps = c(1,2,3),
                                 model_prompts = "Wizard of Oz",
                                 model_seed = 101,
                                 questioned_sessions = 3,
                                 questioned_reps = 1,
                                 questioned_prompts = "Wizard of Oz"))
})

test_that("no writers are in both the template data frame and the model data frame", {
  docs1 <- select_csafe_docs(num_template_writers = 50,
                            template_sessions = 1,
                            template_reps = 1,
                            template_prompts = "London Letter",
                            template_seed = 100,
                            num_model_writers = 40,
                            model_sessions = 1,
                            model_reps = c(1,2,3),
                            model_prompts = "Wizard of Oz",
                            model_seed = 101,
                            questioned_sessions = 3,
                            questioned_reps = 1,
                            questioned_prompts = "Wizard of Oz")
  docs2 <- select_csafe_docs(num_template_writers = 50,
                             template_sessions = c(1,2,3),
                             template_reps = c(1,2,3),
                             template_prompts = "London Letter",
                             template_seed = 200,
                             num_model_writers = 40,
                             model_sessions = 1,
                             model_reps = c(1,2,3),
                             model_prompts = "Wizard of Oz",
                             model_seed = 300,
                             questioned_sessions = 3,
                             questioned_reps = 1,
                             questioned_prompts = "Wizard of Oz")
  expect_equal(length(intersect(docs1$template$writer, docs1$model$writer)), 0)
  expect_equal(length(intersect(docs2$template$writer, docs2$model$writer)), 0)
})

test_that("the writers are the same in the model and questioned data frames", {
  docs <- select_csafe_docs(num_template_writers = 50,
                            template_sessions = 1,
                            template_reps = 1,
                            template_prompts = "London Letter",
                            template_seed = 100,
                            num_model_writers = 40,
                            model_sessions = 1,
                            model_reps = c(1,2,3),
                            model_prompts = "Wizard of Oz",
                            model_seed = 101,
                            questioned_sessions = 3,
                            questioned_reps = 1,
                            questioned_prompts = "Wizard of Oz")
  
  expect_identical(unique(docs$model$writer), unique(docs$questioned$writer))
})

test_that("the documents are different in the model and questioned data frames", {
  docs <- select_csafe_docs(num_template_writers = 50,
                            template_sessions = 1,
                            template_reps = 1,
                            template_prompts = "London Letter",
                            template_seed = 100,
                            num_model_writers = 40,
                            model_sessions = 1,
                            model_reps = c(1,2,3),
                            model_prompts = "Wizard of Oz",
                            model_seed = 101,
                            questioned_sessions = 3,
                            questioned_reps = 1,
                            questioned_prompts = "Wizard of Oz")
  
  expect_equal(length(intersect(docs$questioned$doc, docs$model$doc)), 0)
})

test_that("an error occurs when the same documents are used in the model and questioned data frames", {
  expect_error(select_csafe_docs(num_template_writers = 50,
                                 template_sessions = 1,
                                 template_reps = 1,
                                 template_prompts = "London Letter",
                                 template_seed = 100,
                                 num_model_writers = 40,
                                 model_sessions = 1,
                                 model_reps = c(1,2,3),
                                 model_prompts = "Wizard of Oz",
                                 model_seed = 101,
                                 questioned_sessions = 1,
                                 questioned_reps = 1,
                                 questioned_prompts = "Wizard of Oz"))
})
