#!/usr/bin/env python3

import askcli
import csv
import numpy as np
from matplotlib import pyplot as plt
import sys
import shapefile
import jumble

def read_draw_output(filename):
    """Reads contours produced by Fortran programs test_gcontr or
    test_find_contours_no_coord into a dictionary of contours.

    filename is a string. Keys of the dictionary are indices of
    levels, 1-based. A value of the dictionary is a list of
    contours. Each contour is represented by a Numpy array..

    """

    with open(filename) as draw_output:
        l = {}
        reader = csv.reader(draw_output, delimiter = " ",
                            skipinitialspace = True)

        for row in reader:
            jump, icont, x, y = int(row[0]), int(row[1]), float(row[2]), \
                                float(row[3])
            assert jump == 2 or jump == 3
            level_index = icont
            my_contour = [(x, y)]

            for row in reader:
                jump, icont, x, y = int(row[0]), int(row[1]), float(row[2]), \
                                      float(row[3])
                assert jump in [1, 4, 5] and icont == level_index
                my_contour.append((x, y))
                if jump == 4 or jump == 5: break

            my_contour = np.array(my_contour)

            if level_index in l:
                l[level_index].append(my_contour)
            else:
                l[level_index] = [my_contour]

    return l

def define_example(x, y):
    x, y = np.meshgrid(x, y)
    mu = 0.3
    z = (1 - mu) * (2 / np.sqrt((x - mu)**2 + y**2) + (x - mu)**2 + y**2) \
        + mu * (2 / np.sqrt((x + 1 - mu)**2 + y**2) + (x + 1 - mu)**2 + y**2)
    c = [3, 3.05, 3.2, 3.5, 3.50135, 3.6,  3.766413, 4, 4.130149, 5, 10,
         np.amax(z), 80]
    return z, c

def plot_test_gcontr():
    """Plots contours produced by Fortran program test_gcontr together with
    contours from matplotlib.

    """
    
    xmin = -2
    xmax = 2
    nx = 51
    x = np.linspace(xmin, xmax, nx)
    ymin = -2
    ymax = 2
    ny = 51
    y = np.linspace(ymin, ymax, ny)
    z, c = define_example(x, y)
    filename = input("File containing contours from test_gcontr? ")
    l_gcontr = read_draw_output(filename)
    plt.figure()

    for level_index, contour_list in l_gcontr.items():
        print("level_index =", level_index)
        print("number of contours:", len(contour_list))
        print("level plotted by matplotlib:", c[level_index - 1])

        for my_contour in contour_list:
            plt.plot(my_contour[:, 0] - 1, my_contour[:, 1] - 1,
                     color = "black")

        plt.contour(z, [c[level_index - 1]], colors = "red")
        plt.draw() # necessary with Qt4agg backend, not with Tkagg
        print("Press a key")
        plt.waitforbuttonpress()
        plt.clf()

    plt.contourf(x, y, z, c)
    plt.flag()
    plt.colorbar()
    plt.draw()
    print("Done.")
    plt.show()

def compare_gcontr_find_contours_no_coord():
    """Compares contours produced by Fortran programs test_gcontr and
    test_find_contours_no_coord.

    """
    
    filename = input("File containing contours from test_gcontr? ")
    l_gcontr = read_draw_output(filename)
    l_find_contours = read_draw_output("test_find_contours_no_coord.csv")
    assert len(l_gcontr) == len(l_find_contours)

    for level_index, contour_list_gcontr in l_gcontr.items():
        print("level_index =", level_index)
        contour_list_find_contours = l_find_contours[level_index]
        assert len(contour_list_gcontr) == len(contour_list_find_contours)

        for contour_gcontr in contour_list_gcontr:
            # Remove last (duplicated) point:
            contour_gcontr = np.delete(contour_gcontr, -1, axis = 0)

            # Look for this contour in contour_list_find_contours:

            for contour_find_contours in contour_list_find_contours:
                # Remove last (duplicated) point:
                contour_find_contours = np.delete(contour_find_contours, -1,
                                                  axis = 0)

                if np.size(contour_gcontr, axis = 0) \
                   == np.size(contour_find_contours, axis = 0):

                    first_point = contour_gcontr[0]

                    # Look for first_point in contour_find_contours:
                    m = contour_find_contours == first_point
                    i = np.where(np.logical_and(m[:, 0], m[:, 1]))
                    i = i[0]
                    ##assert np.size(i) <= 1

                    if np.size(i) == 1:
                        contour_find_contours = np.roll(contour_find_contours,
                                                        - i[0], axis = 0)
                        if np.all(contour_find_contours == contour_gcontr):
                            break

                        # Try in the other direction:
                        contour_find_contours = np.flipud(contour_find_contours)
                        contour_find_contours = np.roll(contour_find_contours,
                                                        1, axis = 0)
                        if np.all(contour_find_contours == contour_gcontr):
                            break
            else:
                print("contour_gcontr =", contour_gcontr)
                print("contour_find_contours =", contour_find_contours)
                ##sys.exit("not found")

def plot_test_contours():
    filename = input("Name of file containing x, y (return if regular grid)? ")
    if filename != "":
        d = jumble.read_line_array(filename)
        x = d["x"]
        y = d["y"]
    else:
        x_min = - 2.
        y_min = - 2.
        nx = 51
        ny = 51
        x_max = 2.
        y_max = 2.
        x = np.linspace(x_min, x_max, nx)
        y = np.linspace(y_min, y_max, ny)

    z, c = define_example(x, y)
    filename = input("Shapefile? ")
    sf = shapefile.Reader(filename)
    shapeRecs = sf.iterShapeRecords()
    plt.figure()

    for shapeRec in shapeRecs:
        print("level =", shapeRec.record[0])
        my_contour = np.array(shapeRec.shape.points)
        plt.plot(my_contour[:, 0], my_contour[:, 1], "-o", color = "black")
        plt.contour(x, y, z, [shapeRec.record[0]], colors = "red")
        print("Press a key")
        plt.waitforbuttonpress()
        plt.clf()
        
    ##plt.contour(x, y, z, c)
    plt.close()
    plt.show()
    print("Done.")
            
if __name__ == "__main__":
    m = askcli.Menu(["Plot contours from gcontr or find_contours_no_coord",
                     "Compare gcontr and find_contours_no_coord",
                     "Test contour (regular or irregular)", "Quit"])
    while True:
        m.launch("Choice")
        if m.choose == "1":
            plot_test_gcontr()
        elif m.choose == "2":
            compare_gcontr_find_contours_no_coord()
        elif m.choose == "3":
            plot_test_contours()
        elif m.choose == "4":
            break
