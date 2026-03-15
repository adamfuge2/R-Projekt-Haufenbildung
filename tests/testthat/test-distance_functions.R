
test_that('euclidean distance',{
  distance <- euclidean
  expect_equal(distance(1,0),1)
  expect_equal(distance(c(0,0),c(3,4)),5)
  expect_equal(distance(tibble(X=0,Y=0),tibble(X=3,Y=4)),distance(c(0,0),c(3,4)))
  # symmetry:
  expect_equal(distance(c(0,1,0),c(1,1,4)),distance(c(1,1,4),c(0,1,0)))

  expect_warning(distance(c(1,4),c(1,4,5)))
  expect_error(distance('hallo','hall'))
})

test_that('manhattan distance',{
  distance <- manhattan
  expect_equal(distance(1,0),1)
  expect_equal(distance(c(0,0),c(3,4)),7)
  expect_equal(distance(tibble(X=0,Y=0),tibble(X=3,Y=4)),distance(c(0,0),c(3,4)))
  # symmetry:
  expect_equal(distance(c(0,1,0),c(1,1,4)),distance(c(1,1,4),c(0,1,0)))

  expect_warning(distance(c(1,4),c(1,4,5)))
  expect_error(distance('hallo','hall'))
})

test_that('maximum distance',{
  distance <- maximumDistance
  expect_equal(distance(1,0),1)
  expect_equal(distance(c(0,0),c(3,4)),4)
  expect_equal(distance(tibble(X=0,Y=0),tibble(X=3,Y=4)),distance(c(0,0),c(3,4)))
  # symmetry:
  expect_equal(distance(c(0,1,0),c(1,1,4)),distance(c(1,1,4),c(0,1,0)))

  expect_warning(distance(c(1,4),c(1,4,5)))
  expect_error(distance('hallo','hall'))
})

test_that('canberra distance',{
  distance <- canberra
  expect_equal(distance(1,0),1)
  expect_equal(distance(c(0,1,0),c(1,1,4)),2)
  expect_equal(distance(tibble(X=0,Y=0),tibble(X=3,Y=4)),distance(c(0,0),c(3,4)))
  # symmetry:
  expect_equal(distance(c(0,1,0),c(1,1,4)),distance(c(1,1,4),c(0,1,0)))

  expect_error(distance('hallo','hall'))
})

test_that('binary distance',{
  distance <- binary
  expect_equal(distance(1,0),1)
  expect_equal(distance(c(0,1,0),c(1,1,4)),0.66666667)
  expect_equal(distance(tibble(X=0,Y=0),tibble(X=3,Y=4)),distance(c(0,0),c(3,4)))
  # symmetry:
  expect_equal(distance(c(0,1,0),c(1,1,4)),distance(c(1,1,4),c(0,1,0)))

})

test_that('minkowski distance',{
  distance <- pDistance(4)
  expect_equal(distance(1,0),1)
  expect_equal(distance(c(0,1,0),c(1,1,4)),4.00390054)
  expect_equal(distance(tibble(X=0,Y=0),tibble(X=3,Y=4)),distance(c(0,0),c(3,4)))
  # symmetry:
  expect_equal(distance(c(0,1,0),c(1,1,4)),distance(c(1,1,4),c(0,1,0)))

  expect_warning(distance(c(1,4),c(1,4,5)))

  # different values for p
  expect_no_error(pDistance(1)(c(0,1,0),c(1,1,4)))
  expect_no_error(pDistance(1.5)(c(0,1,0),c(1,1,4)))
  expect_no_error(pDistance(2)(c(0,1,0),c(1,1,4)))
  expect_no_error(pDistance(4)(c(0,1,0),c(1,1,4)))
  expect_no_error(pDistance(100)(c(0,1,0),c(1,1,4)))
})

################# study #################

test_that('study_courses_distance',{
  expect_equal(study_courses_distance('mathematics','biology'),
               study_courses_distance('biology','mathematics'))
  expect_equal(study_courses_distance('mathematics','computer science'),1)
  expect_error(study_courses_distance('fdsagkjgkahfjk','fdashkfh'))
})


############### pacman #####################

test_that('pacman distance',{
  expect_equal(pacman_distance(24)(x = 1, y = 23),2)
  expect_equal(pacman_distance(dim_lengths = c(26,30),
                                  base_distance = manhattan)(x = c(5,2),
                                                             y= c(25,23)),
               15)
})

test_that('hours distance',{
  expect_equal(hours_distance(x = 1, y = 23),2)
  expect_equal(hours_distance(x = 1, y = 23),hours_distance(x = 23, y = 1))

})

test_that('original_pacman_distance',{
  expect_equal(original_pacman_distance(x = c(5,2),
                y= c(25,23)),
               15)
  expect_equal(original_pacman_distance(x = c(5,2),
                                        y = c(25,23)),
               original_pacman_distance(x = c(25,23),
                                        y = c(5,2)))
})

################ distanceByLongitudeAndLatitude #######

test_that('long and lang distance',{

  Paris <- tibble::tibble(latitude=48.8566,longitude=2.3522)
  Krakau <- tibble::tibble(latitude=50.0647,longitude=19.9450)
  Canberra <- tibble::tibble(latitude=-35,longitude=149)
  Atlantis <- tibble::tibble(latitude=-16,longitude=-170)
  expect_equal(distanceByLongitudeAndLatitude(Canberra,Atlantis),
               distanceByLongitudeAndLatitude(Atlantis,Canberra))
  expect_lt(distanceByLongitudeAndLatitude(Canberra,Atlantis),
               distanceByLongitudeAndLatitude(Paris,Canberra))
})
