devtools::load_all()


# main_dir <- 'examples/example_template/data/template_docs'
main_dir <- '/Users/stephanie/Documents/handwriting_datasets/CSAFE_docs'
# main_dir <- '/Users/stephanie/Documents/handwriting_datasets/testing'
# main_dir <- '/Users/stephanie/Documents/version_control/handwriter_copies/docs/w0720_copies_cropped'
files <- list.files(main_dir, full.names = TRUE)

file <- files[28]

doc1 <- processDocument_by_component(file)
doc2 <- processDocument(file)

# plot nodes
plotNodes(doc1, nodeSize=1)
ggsave(file.path('examples', 'profiles', stringr::str_replace(basename(file), '.png', '_by_comp_nodes.png')), width=10, height=10)
plotNodes(doc2, nodeSize=1)
ggsave(file.path('examples', 'profiles', stringr::str_replace(basename(file), '.png', '_by_doc_nodes.png')), width=10, height=10)

# plot breaks
plotNodes(doc1, plot_break_pts = TRUE, nodeSize=3, nodeColor = 'green')
ggsave(file.path('examples', 'profiles', stringr::str_replace(basename(file), '.png', '_by_comp_breaks.png')), width=10, height=10)
plotNodes(doc2, plot_break_pts = TRUE, nodeSize=3, nodeColor = 'green')
ggsave(file.path('examples', 'profiles', stringr::str_replace(basename(file), '.png', '_by_doc_breaks.png')), width=10, height=10)
