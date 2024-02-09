
test_that("Should generate numbers on first level", {
  pn <- PageNumbering$new()
  
  expect_equal("1.", pn$AddChild())
  expect_equal("2.", pn$AddChild())
  expect_equal("3.", pn$AddChild())
  expect_equal("4.", pn$AddChild())
})

test_that("Should generate letters on second level", {
  pn <- PageNumbering$new()
  
  pn$AddChild()
  pn$DiveLevel()
  
  expect_equal("1a.", pn$AddChild())
  expect_equal("1b.", pn$AddChild())
  expect_equal("1c.", pn$AddChild())
  expect_equal("1d.", pn$AddChild())
})

test_that("Should generate roman numerals on third level", {
  pn <- PageNumbering$new()
  
  pn$AddChild()
  pn$DiveLevel()
  pn$AddChild()
  pn$DiveLevel()
  
  expect_equal("1a i.", pn$AddChild())
  expect_equal("1a ii.", pn$AddChild())
  expect_equal("1a iii.", pn$AddChild())
  expect_equal("1a iv.", pn$AddChild())
})

test_that("Should return to generating numbers on fourth level", {
  pn <- PageNumbering$new()
  
  pn$AddChild()
  pn$DiveLevel()
  pn$AddChild()
  pn$DiveLevel()
  pn$AddChild()
  pn$DiveLevel()
  
  expect_equal("1a i-1.", pn$AddChild())
  expect_equal("1a i-2.", pn$AddChild())
  expect_equal("1a i-3.", pn$AddChild())
  expect_equal("1a i-4.", pn$AddChild())
})

test_that("Dive then float should maintain level", {
  pn <- PageNumbering$new()
  
  pn$AddChild()
  pn$DiveLevel()
  pn$FloatLevel()
  
  expect_equal("2.", pn$AddChild())
})

test_that("Floating up a level should resume sequence", {
  pn <- PageNumbering$new()
  
  pn$AddChild()
  pn$DiveLevel()
  pn$AddChild()
  pn$AddChild()
  pn$FloatLevel()
  
  expect_equal("2.", pn$AddChild())
})

test_that("Diving into level with no children should error", {
  pn <- PageNumbering$new()
  expect_error(pn$DiveLevel())
})

test_that("Floating above root should error", {
  pn <- PageNumbering$new()
  expect_error(pn$FloatLevel())
})
