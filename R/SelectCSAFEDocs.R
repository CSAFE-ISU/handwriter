#' select_csafe_docs
#'
#' Build three sets of documents from the CSAFE Handwriting Database: cluster
#' template training documents; model training documents; and questioned (test)
#' documents. The CSAFE Handwriting Database contains 200+ writers. Each writer
#' wrote three prompts: the London letter, a common phrase, and an excerpt from
#' the Wizard of Oz. Each writer wrote each prompt three times during three
#' different sessions. Each session was at least three weeks apart.
#'
#' The `select_csafe_docs` function allows the user to select the number of
#' writers to use for the template and model, and ensures that no writers are
#' used in both sets. The model implemented in handwriter is only applicable to
#' closed-sets of writers: the questioned document is known to have been written
#' by one writers whose known handwriting samples are used to fit the model. The
#' `select_csafe_docs` function selects handwriting samples from the model
#' writers for use as questioned documents. While the writers are the same, the
#' a document cannot be used in both the model and the questioned sets. If a
#' user attempts to do this, `select_csafe_docs` will return an error message.
#'
#' The `select_csafe_docs` function also allows the user to choose which
#' prompt(s), session(s), and repetition(s) to use for each of the three sets.
#' As already mentioned, the function won't allow the user to select the same
#' documents for the model and questioned sets.
#'
#' @param num_template_writers An integer number of writers to randomly select
#'   for cluster template creation.
#' @param template_sessions Vector of writing session(s) to use for cluster template creation
#' @param template_reps Vector of writing repetition(s) to use for cluster template creation
#' @param template_prompts Vector of writing prompt(s) to use for cluster template creation
#' @param template_seed An integer seed for randomly selecting writers to use for cluster template creation
#' @param num_model_writers An integer number of writers to randomly select
#'   for fitting a model
#' @param model_sessions Vector of writing session(s) to use for fitting a model
#' @param model_reps Vector of writing repetitions(s) to use for fitting a model
#' @param model_prompts Vector of writing prompt(s) to use for fitting a model
#' @param model_seed An integer seed for randomly selecting writers to use for fitting a model
#' @param questioned_sessions Vector of writing session(s) to use for questioned (test) documents
#' @param questioned_reps Vector of writing repetitions(s) to use for questioned (test) documents
#' @param questioned_prompts Vector of writing prompts(s) to use for questioned (test) documents
#'
#' @return A list of data frames
#' @export
#'
#' @examples
#' docs <- select_csafe_docs(
#'   num_template_writers = 10,
#'   template_sessions = 1,
#'   template_reps = 1,
#'   template_prompts = "London Letter",
#'   template_seed = 100,
#'   num_model_writers = 5,
#'   model_sessions = 1,
#'   model_reps = c(1, 2, 3),
#'   model_prompts = "Wizard of Oz",
#'   model_seed = 101,
#'   questioned_sessions = 3,
#'   questioned_reps = 1,
#'   questioned_prompts = "Wizard of Oz"
#' )
#'
#' @md
select_csafe_docs <- function(num_template_writers,
                              template_sessions,
                              template_reps,
                              template_prompts,
                              template_seed,
                              num_model_writers,
                              model_sessions,
                              model_reps,
                              model_prompts,
                              model_seed,
                              questioned_sessions,
                              questioned_reps,
                              questioned_prompts) {
  if ((num_template_writers + num_model_writers) > length(unique(csafe_docs$writer))) {
    stop(paste(
      "The sum of num_tempalte_writers and num_model_writers must be less than or equal to",
      length(unique(csafe_docs$writer))
    ))
  }

  # get template docs
  template_df <- select_template_docs(
    num_writers = num_template_writers,
    sessions = template_sessions,
    reps = template_reps,
    prompts = template_prompts,
    seed = template_seed
  )

  # get model docs - exclude template writers
  model_df <- select_model_docs(
    template_df = template_df,
    num_writers = num_model_writers,
    sessions = model_sessions,
    reps = model_reps,
    prompts = model_prompts,
    seed = model_seed
  )

  # get questioned docs
  questioned_df <- select_questioned_docs(
    model_df = model_df,
    sessions = questioned_sessions,
    reps = questioned_reps,
    prompts = questioned_prompts
  )

  list("template" = template_df, "model" = model_df, "questioned" = questioned_df)
}



#' select_template_docs
#'
#' Randomly select template training documents to create a clustering template.
#'
#' @param num_writers Integer number of writers to randomly select
#' @param sessions Vector of writing session(s) to use
#' @param reps Vector of writing repetition(s) to use
#' @param prompts Vector of writing prompt(s) to use
#' @param seed Integer seed for the random number generator
#'
#' @return A data frame of selected documents
#'
#' @export
select_template_docs <- function(num_writers, sessions, reps, prompts, seed) {
  # format sessions, prompts, and reps
  sessions <- paste0("s0", sessions)
  reps <- paste0("r0", reps)
  prompts <- sapply(prompts, get_prompt_code, USE.NAMES = FALSE)

  # filter sessions, prompts, and reps
  df <- csafe_docs %>% dplyr::filter(session %in% sessions, prompt %in% prompts, repetition %in% reps)

  # randomly sample writers
  set.seed(seed = seed)
  writers <- sample(unique(df$writer), size = num_writers)
  df %>% dplyr::filter(writer %in% writers)
}

#' select_model_docs
#'
#' Randomly select model training documents to fit a model. The model writers
#' will be distinct from the template training writers.
#'
#' @param template_df A data frame of template training documents created with
#'   [`select_template_docs`]
#' @param num_writers Integer number of writers to randomly select
#' @param sessions Vector of writing session(s) to use
#' @param reps Vector of writing repetition(s) to use
#' @param prompts Vector of writing prompt(s) to use
#' @param seed Integer seed for the random number generator
#'
#' @return A data frame of selected documents
#'
#' @export
select_model_docs <- function(template_df, num_writers, sessions, reps, prompts, seed) {
  # drop template writers
  df <- csafe_docs %>% dplyr::filter(!(writer %in% unique(template_df$writer)))

  # filter model sessions, prompts, and reps
  # format sessions, prompts, and reps
  sessions <- paste0("s0", sessions)
  reps <- paste0("r0", reps)
  prompts <- sapply(prompts, get_prompt_code, USE.NAMES = FALSE)
  df <- df %>% dplyr::filter(session %in% sessions, repetition %in% reps, prompt %in% prompts)

  # randomly sample writers
  set.seed(seed = seed)
  writers <- sample(unique(df$writer), size = num_writers)
  df %>% dplyr::filter(writer %in% writers)
}

#' select_questioned_docs
#'
#' Randomly select questioned documents from the same writers used to fit the model.
#'
#' @param model_df A data frame of model training documents created with
#'   [`select_model_docs`]
#' @param sessions Vector of writing session(s) to use
#' @param reps Vector of writing repetition(s) to use
#' @param prompts Vector of writing prompt(s) to use
#'
#' @return A data frame of selected documents
#'
#' @export
select_questioned_docs <- function(model_df, sessions, reps, prompts) {
  # format sessions, prompts, and reps
  sessions <- paste0("s0", sessions)
  reps <- paste0("r0", reps)
  prompts <- sapply(prompts, get_prompt_code, USE.NAMES = FALSE)

  # filter documents
  df <- csafe_docs %>% dplyr::filter(
    writer %in% unique(model_df$writer),
    session %in% sessions,
    repetition %in% reps,
    prompt %in% prompts
  )

  if (length(intersect(df$doc, model_df$doc)) > 0) {
    stop("You must choose questioned documents that were not used to fit the model.")
  }

  return(df)
}

#' get_prompt_code
#'
#' Return the prompt code
#'
#' @param x A prompt name as a string. Examples: "Common Phrase"; "Wizard of
#'   Oz"; and "London Letter"
#'
#' @return A prompt code as a string
#'
#' @noRd
get_prompt_code <- function(x) {
  # if use supplied the correct code, simply return it
  if (x %in% c("pLND", "pPHR", "pWOZ")) {
    return(x)
  }

  # find the appropriate code or return a message
  x <- tolower(x)
  if (x == "common phrase" | x == "phrase" | x == "phr") {
    code <- "pPHR"
    return(code)
  } else if (x == "wizard of oz" | x == "woz") {
    code <- "pWOZ"
    return(code)
  } else if (x == "london letter" | x == "lnd") {
    code <- "pLND"
    return(code)
  } else {
    stop(paste(x, "is not a CSAFE prompt."))
  }
}

#' copy_CSAFE_docs
#'
#' The user can build 3 sets - template training, model training, and questioned
#' (test) - of documents from the CSAFE Handwriting Database with the function
#' [`select_csafe_docs`]. The output of `select_csafe_docs` is a list of three
#' data frames - template, model, and questioned. Each data frame contains the
#' file names of CSAFE documents assigned to either the template, model, or
#' questioned set. The `copy_csafe_docs` function will copy these sets of
#' documents from the directory containing the downloaded CSAFE Handwriting
#' Database to the new directories.
#'
#' @param docs A list of CSAFE documents created by [`select_csafe_docs`]
#' @param input_dir Path to directory containing the downloaded CSAFE
#'   Handwriting Database
#' @param template_dir Path to directory where template training documents will
#'   be copied
#' @param model_dir Path to directory where model training documents will be
#'   copied
#' @param questioned_dir Path to
#'
#' @return Message "Documents have been copied."
#' @export
#'
#' @examples
#' \dontrun{
#' docs <- select_csafe_docs(
#'   num_template_writers = 10,
#'   template_sessions = 1,
#'   template_reps = 1,
#'   template_prompts = "London Letter",
#'   template_seed = 100,
#'   num_model_writers = 5,
#'   model_sessions = 1,
#'   model_reps = c(1, 2, 3),
#'   model_prompts = "Wizard of Oz",
#'   model_seed = 101,
#'   questioned_sessions = 3,
#'   questioned_reps = 1,
#'   questioned_prompts = "Wizard of Oz"
#' )
#'
#' input_dir <- path / to / downloaded_CSAFE_docs
#' output_template_dir <- path / to / template_docs_output
#' output_model_dir <- path / to / model_docs_output
#' output_questioned_dir <- path / to / questioned_docs_output
#'
#' copy_csafe_docs(
#'   docs = docs,
#'   input_dir = input_dir,
#'   template_dir = output_template_dir,
#'   model_dir = output_model_dir,
#'   questioned_dir = output_questioned_dir
#' )
#' }
#'
#' @md
copy_csafe_docs <- function(docs, input_dir, template_dir, model_dir, questioned_dir) {
  # create output folders
  if (!dir.exists(template_dir)) {
    dir.create(template_dir, recursive = TRUE)
  }
  if (!dir.exists(model_dir)) {
    dir.create(model_dir, recursive = TRUE)
  }
  if (!dir.exists(questioned_dir)) {
    dir.create(questioned_dir, recursive = TRUE)
  }

  # copy files
  file.copy(file.path(input_dir, docs$template$doc), file.path(template_dir, docs$template$doc))
  file.copy(file.path(input_dir, docs$model$doc), file.path(model_dir, docs$model$doc))
  file.copy(file.path(input_dir, docs$questioned$doc), file.path(questioned_dir, docs$questioned$doc))

  message("Documents have been copied.")
}
