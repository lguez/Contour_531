module polyline_m

  implicit none

  type polyline
     integer n_points
     logical closed ! last point == first point, both are stored
     real, allocatable:: points(:, :) ! (2, n_points)
  end type polyline

end module polyline_m
