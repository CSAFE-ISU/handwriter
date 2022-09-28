# handwriter pipeline using example data stored in the package

# fit model ----
md <- format_model_data(proc_list=example_model_proc_list, 
                        writer_indices=c(2,5), 
                        doc_indices=c(7,18), 
                        a=2, b=0.25, c=2, d=2, e=0.5)
emd <- example_model_training_data
draws <- fit_model(md, num_iters = 4000)
draws <- drop_burnin(draws, 1000)

# questioned documents ----
qd <- format_questioned_data(proc_list = example_questioned_data,
                             writer_indices=c(2,5), 
                             doc_indices=c(7,18))

