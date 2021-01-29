library(testthat)
library(digest)

test_1.1 <- function(){
    test_that("intro_q1_1", {
        expect_false("C" %in% answer1.1)
        expect_false("D" %in% answer1.1)
    })
print("success!")
}

test_1.2 <- function(){
    test_that("intro_q2_test", {
        expect_equal(digest(answer1.2), 'd2a90307aac5ae8d0ef58e2fe730d38b')
    })
print("success!")
}

test_1.3 <- function(){
    test_that("intro_q2_test", {
        expect_equal(digest(answer1.3), 'd2a90307aac5ae8d0ef58e2fe730d38b')
    })
print("success!")
}

test_2.1 <- function(){
    test_that("func_q1_question", {
        expect_true("package:tidyverse" %in% search())
        })
print("success!")
    }

test_2.2 <- function(){
    test_that("crossword_answer", {
        expect_equal(digest(a3), '37170e4ebefbbc99524e24c6ec18e28e') 
        expect_equal(digest(a5), 'b06cc0e314bc6be124b186ed3b53a3b8') 
        expect_equal(digest(a6), '66e130ded1307748093dae2a30bcbd71') 
        expect_equal(digest(a8), 'da937026ff415d4e97938abca1997512') 
        expect_equal(digest(a10), '1a22317ee8b5e9bf8220c2e74e36a420') 
        expect_equal(digest(a13), 'e3c0d8c0204cb6ceeb4bb562ad863ef4')
        expect_equal(digest(a14), '4a68f10c4c601965dbac9579c52dbedc') 
        expect_equal(digest(a15), 'e8638001643d3c4847b2a6ff3765a3ed') 
        expect_equal(digest(a16), 'b5a42577f646ae755d221ec99f16959b')
        expect_equal(digest(d1), '2346301e69d76ae86dd0a6ffde3aa238')
        expect_equal(digest(d2), '37dfc540649b20608f6cd5abd2adad5b')
        expect_equal(digest(d4), '5c495360444d07a2cde82dbc5cef2b08')
        expect_equal(digest(d5), '0056b3386d92d2618be48c84e3d514b4')
        expect_equal(digest(d7), '69170f3023182e2b866acca13d9f11ad')
        expect_equal(digest(d9), '4a68f10c4c601965dbac9579c52dbedc')
        expect_equal(digest(d11), '2479b6033cac31e1e5ab9c406ffc1ad6')
        expect_equal(digest(d12), 'a05534ea8144867bd3944a7f0295b7bf')
          })
print("Success!")
}

test_3.1 <- function(){
    test_that("3.1 is correct", {
        expect_true(exists("answer3.1"))
        expect_equal(digest(answer3.1), 'c1f86f7430df7ddb256980ea6a3b57a4')
    })
print("Success!")
}

test_3.2 <- function(){
    test_that("3.2 is correct", {
        expect_true(exists("answer3.2"))
        expect_equal(digest(answer3.2), 'd2a90307aac5ae8d0ef58e2fe730d38b')
    })
print("Success!")
}

test_3.3 <- function(){
    test_that("created an object called 'mri_data'", {
        expect_true(exists("mri_data"))
    })
    test_that("mri_data is in a data frame", {
        expect_true("data.frame" %in% class(mri_data))
    })
    test_that("3.2 solution is correct", {
        expect_equal(digest(mri_data), 'fc803b44e2b2eda94a40b4e7ef6d567b')
    })
    test_that("mri_data contains the correct information", {
        expect_equal(digest(dim(mri_data)), 'a231b28497c805affacf0c0fdb41fe62')
        expect_equal(digest(mean(mri_data$ASF, na.rm = TRUE)), 'ae026ee3a9ac4b2a4794a05d670cb3fe')
        expect_equal(colnames(mri_data), c("ID", "M/F", "Hand", "Age", "Educ", 
                                           "SES", "MMSE", "CDR", "eTIV", "nWBV", 
                                          "ASF", "Delay"))
    })
    test_that("col are the correct variable type", {
        expect_equal(class(mri_data$ID), 'character')
        expect_equal(class(mri_data$SES), 'numeric')
    })
print("Success!")
}

test_3.4 <- function(){
    test_that("3.4 solution is correct", {
        expect_equal(digest(answer3.4), 'be0366bf3ab7c3a8343db6c2a13cfc4d')
    })
}

test_4.1 <- function() {
    test_that("created a new object called answer4.1", {
        expect_true(exists("answer4.1"))
    })
    test_that("answer4.1 contains the correct dimensions", {
        expect_equal(digest(nrow(answer4.1)), '30e0a5990c97a18dd3b85b5f837c84')
        expect_equal(digest(ncol(answer4.1)), '4b5630ee914e848e8d07221556b0a2fb')
    })
    test_that("answer4.1 contains the Age col", {
        expect_true("Age" %in% colnames(answer4.1))
    })
    test_that("answer4.1 contains the correct data", {
        expect_equal(digest(sum(answer4.1$Age)), '67b95465ea660abe2ca47e3583a7fee3')
        expect_equal(digest(mean(answer4.1$Age, na.rm = TRUE)), 'e49bdd3faaef85c3dd41ec3647a61768')
    })
print("Success!")
}

test_4.2 <- function() {
    test_that("created a new object called answer4.2", {
        expect_true(exists("answer4.2"))
    })
    test_that("answer4.2 contains the correct dimensions", {
        expect_equal(digest(nrow(answer4.2)), '30e0a5990c97a18d60d3b85b5f837c84')
        expect_equal(digest(ncol(answer4.2)), '25e6a154090e35101d7678d6f034353a')
    })
    test_that("answer4.2 contains the Age and SES col", {
        expect_true("Age" %in% colnames(answer4.2))
        expect_true("SES" %in% colnames(answer4.2))
    })
    test_that("answer4.2 contains the correct data", {
        expect_equal(digest(sum(answer4.2$Age)), '67b95465ea660abe2ca47e3583a7fee3')
        expect_equal(digest(mean(answer4.2$Age, na.rm = TRUE)), 'e49bdd3faaef85c3dd41ec3647a61768')
        expect_equal(digest(sum(answer4.2$SES)), '9c9393e1464352cd4fbea94dfadfa02a')
        expect_equal(digest(sum(answer4.2$Educ)), '9c9393e1464352cd4fbea94dfadfa02a')
    })
    test_that("answer4.2 columns are the correct variable type", {
        expect_equal(class(answer4.2$Age), 'numeric')
        expect_equal(class(answer4.2$ID), 'character')
    })
print("Success!")
}

test_4.3 <- function() {
    test_that("created a new object called answer4.3", {
        expect_true(exists("answer4.3"))
    })
    test_that("answer4.3 contains the correct dimensions", {
        expect_equal(digest(nrow(answer4.3)), '30e0a5990c97a18d60d3b85b5f837c84')
        expect_equal(digest(ncol(answer4.3)), '8f3571abef23f6aca0f7b8666a74e7e0')
    })
    test_that("answer4.3 contains the Age and SES col", {
        expect_true("Age" %in% colnames(answer4.3))
        expect_true("ASF" %in% colnames(answer4.3))
    })
    test_that("answer4.3 contains the correct data", {
        expect_equal(digest(sum(answer4.3$ASF)), 'c07914f23cd670eea3aefe749a64c0df')
        expect_equal(digest(mean(answer4.3$ASF, na.rm = TRUE)), 'ae026ee3a9ac4b2a4794a05d670cb3fe')
        expect_equal(digest(sum(answer4.3$nWBV)), 'e15a058376c3d5a03237611046fe33c8')
        expect_equal(digest(mean(answer4.3$nWBV, na.rm = TRUE)), 'ffdbbd12a674bbc38bfb6ed5f3cc0118')
    })
    test_that("answer4.3 columns are the correct variable type", {
        expect_equal(class(answer4.3$Age), 'numeric')
        expect_equal(class(answer4.3$ID), 'character')
        expect_equal(class(answer4.3$ASF), 'numeric')
    })
print("Success!")
}
    
test_4.4 <- function() {
    test_that("created a new object called answer4.4", {
        expect_true(exists("answer4.4"))
    })
    test_that("answer4.4 contains the renamed col", {
        expect_true("gender" %in% colnames(answer4.4))
    })
    test_that("answer4.4 contains the correct data", {
        expect_equal(digest(names(answer4.4)), '967eb290981190cc72e3d2ff9a95192e')
        expect_equal(digest(dim(answer4.4)), 'bb9c9679af44f56913e8a47279ee5aca')
    })
print("Success!")
}

test_4.5 <- function() {
    test_that("created a new object called answer4.5", {
        expect_true(exists("answer4.5"))
    })
    test_that("answer4.5 contains the correct data", {
        expect_equal(digest(sum(pull(answer4.5, Age))), '5ed4344e326f0ac001624045d446e78a')
        expect_equal(digest(dim(answer4.5)), '6981362e55b33765ad6c655e2f5ac446')
    })
print("Success!")
}

test_4.6 <- function() {
    test_that("created a new object called answer4.6", {
        expect_true(exists("answer4.6"))
    })
    test_that("answer4.6 contains the correct data", {
        expect_equal(digest(sum(pull(answer4.6, gender) == "F")),'125d7519071575f4c5611228dfc79364')
        expect_equal(digest(dim(answer4.6)), '671bb1dea653fd823aad96ebf6d5c3e8')
    })
print("Success!")
}

test_4.7 <- function() {
    test_that("created a new object called answer4.7", {
        expect_true(exists("answer4.7"))
    })
        test_that("answer4.7 contains new risk_score column", {
        expect_true("risk_score" %in% colnames(answer4.7))
    })
    test_that("answer4.6 contains the correct data", {
        expect_equal(digest(sum(answer4.7$risk_score, na.rm = TRUE)), '964b95c2164b90385d0af50f7e784b5f')
        expect_equal(digest(mean(answer4.7$risk_score, na.rm = TRUE)), 'bc2c902ddc2953e9b46583d86324f72e')
        expect_equal(digest(dim(answer4.7)), '5205d371030049c4a7d28579ce53697b')
    })
print("Success!")
}

test_5.1 <- function() {
    test_that("created a new object called mri_tidy", {
        expect_true(exists("mri_tidy"))
    })
    test_that("mri_tidy contains new risk_score and gendercolumn", {
        expect_true("risk_score" %in% colnames(mri_tidy))
        expect_true("gender" %in% colnames(mri_tidy))
    })
    test_that("mri_tidy contains the correct data", {
        expect_equal(digest(sum(mri_tidy$risk_score, na.rm = TRUE)), '33908a3a4b24fb16391338dfffe8b4ee')
        expect_equal(digest(sum(pull(mri_tidy, gender) == "F")), '9a9c69f0e6a752abcaf253893a2f8efc')
        expect_equal(digest(sum(mri_tidy$Age, na.rm = TRUE)), '3366d2e90d87151dc8701b8c151cd66f')
    })
print("Success!")
}

test_5.2 <- function() {
    test_that("Created a boxplot called answer5.2", {
        expect_true(exists("answer5.2"))
    })
    test_that("answer5.2 should be a boxplot", {
        expect_true("GeomBoxplot" %in% c(class(answer5.2$layers[[1]]$geom)))
    })
    test_that("axes labels and legend are descriptive and human readable", {
        expect_false(answer5.2$labels$x == "CDR")
        expect_false(answer5.2$labels$y == "MMSE")
        expect_false(answer5.2$labels$fill == "gender")
    })
    test_that("labs are labeled accordingly", {
        expect_equal(answer5.2$labels$x, "Clinical Dementia Score")
        expect_equal(answer5.2$labels$y, "Mini Mental State Examination")
        expect_equal(answer5.2$labels$fill, "Gender")
        expect_equal(answer5.2$labels$title, "MRI and Alzheimer's")
    })
    test_that("CDR is on the x axis", {
        expect_true("as.factor(CDR)" == rlang::get_expr(answer5.2$mapping$x))
    })
    test_that("MMSE is on y axis", {
        expect_true("MMSE" == rlang::get_expr(answer5.2$mapping$y))
    })
    test_that("gender is in the fill argument", {
        expect_true("gender" == rlang::get_expr(answer5.2$mapping$fill))
    })
    test_that("alpha is set at 0.8", {
        expect_equal(answer5.2$layers[[1]]$geom_params$alpha, 0.8)
    })
cat("Success!")
}

test_6.1 <- function(){
    test_that("created an object called 'tbi_age'", {
        expect_true(exists("tbi_age"))
    })
    test_that("tbi_age is in a data frame", {
        expect_true("data.frame" %in% class(tbi_age))
    })
    test_that("question 6.1 solution is correct", {
        expect_equal(digest(tbi_age), 'ba92e9e199aee2365211f17adc10e479')
    })
    test_that("tbi_age contains the correct information",{
        expect_equal(digest(dim(tbi_age)), 'b8e7663c79154e88d4033bfeefadaee0')
        expect_equal(digest(sum(tbi_age$number_est, na.rm = TRUE)), '73ffe56e775ebf724d2dc46258f5e712')
        expect_equal(digest(mean(tbi_age$number_est, na.rm = TRUE)), '80cfbd174c24195955a3c496781b5ee7')
    })
    test_that("tbi_age contains the correct columns", {
        expect_true("age_group" %in% colnames(tbi_age))
    })
    test_that('tbi_age columns are the correct variable type', {
        expect_equal(class(tbi_age$type), 'character')
        expect_equal(class(tbi_age$number_est), 'numeric')
    })
cat("Success!")
}

test_6.2 <- function() {
    test_that("Created a new object called tbi_age_tidy", {
        expect_true(exists("tbi_age_tidy"))
    })
    test_that("tbi_age_tidy contains the correct number of columns and rows", {
        expect_equal(digest(dim(tbi_age_tidy)), 'a9b6497a6216ec4ebfe794fd75526295')
    })
    # testing the mutate manipulations: 
    test_that("Created a new column called pct and is a float", {
        expect_true("pct" %in% colnames(tbi_age_tidy))
        expect_equal(class(tbi_age_tidy$pct), 'numeric')
    })
    test_that("The new column pct contains the correct information", {
        expect_equal(digest(tbi_age_tidy$pct), 'e7c9dc4f27d19ac65ca704627f86e793')
        expect_equal(digest(sum(tbi_age_tidy$pct)), '2522027d230e3dfe02d8b6eba1fd73e1')
    })
    test_that("age_group, type, injury mechanism are factors", {
        expect_equal(class(tbi_age_tidy$age_group), 'factor')
        expect_equal(class(tbi_age_tidy$type), 'factor')
        expect_equal(class(tbi_age_tidy$injury_mechanism), 'factor')
    })
    test_that("age_group is in the ascending order", {
        expect_equal(digest(tbi_age_tidy$age_group), 'a13555e73bcd75359ca82a2206535b2b')
    })
    # testing the filter manipulations:
    test_that("0-17 and Total are not in age_group", {
        expect_false("0-17" %in% levels(tbi_age_tidy$age_group))
        expect_false("Total" %in% levels(tbi_age_tidy$age_group))
    })
    test_that("Other or no mechanism specified is not in injury_mechanism", {
        expect_equal(digest(tbi_age_tidy$injury_mechanism), '23f799b9d8336f8d08c39eed3cbde389')
        expect_false("Other or no mechanism specified" %in% levels(tbi_age_tidy$injury_mechanism))
    })
    test_that("Deaths is the only value in type", {
        expect_equal(levels(tbi_age_tidy$type), 'Deaths')
        expect_equal(digest(tbi_age_tidy$type), 'b7909a8570520a6a4a554a3384e8fdc4')
    })
    # testing to see there are any na values 
    test_that("there are no n/a values", {
        expect_false(TRUE %in% is.na(tbi_age_tidy$rate_est))
        expect_false(TRUE %in% is.na(tbi_age_tidy$number_est))
        expect_equal(any(is.na(tbi_age_tidy)), FALSE)
    })
cat("Success!")
}