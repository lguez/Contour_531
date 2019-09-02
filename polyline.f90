module polyline_m

  implicit none

  type polyline
     integer n_points
     logical closed ! last point == first point, both are stored
     real, allocatable:: points(:, :) ! (2, n_points)
  end type polyline

contains

  pure type(polyline) function null_polyline()

    null_polyline%n_points = 0
    null_polyline%closed = .false.
    allocate(null_polyline%points(2, 0))

  end function null_polyline

end module polyline_m
