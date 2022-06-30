#==================================================================
#======================= K-MEANS CLUSTERING =======================
#==================================================================


#To delete after tab is done  
output$cluster_graphs <- renderImage({list(src = file.path("images/graphs.png"), contentType = "image/png")}, deleteFile = FALSE)
output$cluster_unknown <- renderImage({list(src = file.path("images/unknown_writer_cluster_counts.png"), width = 1200, height = 375, contentType = "image/png")}, deleteFile = FALSE)
output$cluster_1 <- renderImage({list(src = file.path("images/writer1_cluster_counts.png"), width = 1200, height = 375, contentType = "image/png")}, deleteFile = FALSE)
output$cluster_2 <- renderImage({list(src = file.path("images/writer2_cluster_counts.png"), width = 1200, height = 375, contentType = "image/png")}, deleteFile = FALSE)
output$cluster_3 <- renderImage({list(src = file.path("images/writer3_cluster_counts.png"), width = 1200, height = 375, contentType = "image/png")}, deleteFile = FALSE)