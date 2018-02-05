gcontr.o : mark1.o iget.o 
contour_531.o : polyline.o null_polyline.o gcontr.o find_contours_reg_grid.o find_contours_no_coord.o find_contours_irreg_grid.o convert_to_reg_coord.o convert_to_irreg_coord.o convert_to_ind.o 
find_contours_irreg_grid.o : polyline.o find_contours_no_coord.o convert_to_irreg_coord.o 
find_contours_no_coord.o : polyline.o gcontr.o draw_to_scratch.o 
find_contours_reg_grid.o : polyline.o find_contours_no_coord.o convert_to_reg_coord.o 
null_polyline.o : polyline.o 
