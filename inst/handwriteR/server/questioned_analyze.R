q_docs_path <- directoryServer("q_docs")
directoryContentsServer("q_docs_list", q_docs_path)

# settings
q_writer_ind <- substringIndicesServer("q_writer_indices")
substringsServer("q_writers", q_docs_path, q_writer_ind, "writers")
q_doccode_ind <- substringIndicesServer("q_doccode_indices")
substringsServer("q_doccodes", q_docs_path, q_doccode_ind, "doc code")

# select and display image
displayImageServer("q_image", q_docs_path)