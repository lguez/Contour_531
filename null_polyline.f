module null_polyline_m

  implicit none

contains

  pure type(polyline) function null_polyline()

    use polyline_m, only: polyline

    !----------------------------------------------

    null_polyline%n_points = 0
    null_polyline%closed = .false.
    allocate(null_polyline%points(2, 0))

  end function null_polyline

end module null_polyline_m
