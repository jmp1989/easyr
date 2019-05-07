https://stackoverflow.com/questions/18852395/writing-png-plots-into-a-pdf-file-in-r


ezr.png2pdf = function(file_name = 'images_to_pdf.pdf', ncols=2, folder_name=getwd()){

    # set directry
    setwd(folder_name)

    plots = list.files(pattern="*.png")


    pdf(file_name)
    do.call(gridExtra::grid.arrange, c(plots, ncol=ncols))
}