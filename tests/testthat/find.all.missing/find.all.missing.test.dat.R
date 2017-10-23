hold.trait = vector("list")
hold.cov = vector("list")

utrait = names(required.covariates)

# required.covariates[[utrait[1]]]

hold.trait[[1]] = data.frame(trait_id = c(1,2,3,4,5,6),variable_id = c(213,213,213,213,213,213),
                             updated_at = c("1857-03-27","1857-03-27","1890-02-17","1890-02-17","1900-01-13","1900-01-13"))
hold.cov[[1]] = data.frame(trait_id = c(1,3,5),variable_id = c(81,81,81))

# required.covariates[[utrait[2]]]

hold.trait[[2]] = data.frame(trait_id = c(7,8,9,10,11,12),variable_id = c(7,7,7,7,7,7),
                             updated_at = c("1857-03-27","1857-03-27","1890-02-17","1890-02-17","1900-01-13","1900-01-13"))
hold.cov[[2]] = data.frame(trait_id = c(7,9,11),variable_id = c(81,81,81))

# required.covariates[[utrait[3]]]

hold.trait[[3]] = data.frame(trait_id = c(13,14,15,16,17,18),variable_id = c(416,416,416,416,416,416),
                             updated_at = c("1857-03-27","1857-03-27","1890-02-17","1890-02-17","1900-01-13","1900-01-13"))
hold.cov[[3]] = data.frame(trait_id = c(13,15,17),variable_id = c(81,81,81))

# required.covariates[[utrait[4]]]

hold.trait[[4]] = data.frame(trait_id = c(19,20,21,22,23,24),variable_id = c(350,350,350,350,350,350),
                             updated_at = c("1857-03-27","1857-03-27","1890-02-17","1890-02-17","1900-01-13","1900-01-13"))
hold.cov[[4]] = data.frame(trait_id = c(19,21,23),variable_id = c(81,81,81))

# required.covariates[[utrait[5]]]

hold.trait[[5]] = data.frame(trait_id = c(25,26,27,28,29,30),variable_id = c(351,351,351,351,351,351),
                             updated_at = c("1857-03-27","1857-03-27","1890-02-17","1890-02-17","1900-01-13","1900-01-13"))
hold.cov[[5]] = data.frame(trait_id = c(25,27,29),variable_id = c(81,81,81))

# required.covariates[[utrait[6]]]

hold.trait[[6]] = data.frame(trait_id = c(31,32,33,34,35,36),variable_id = c(392,392,392,392,392,392),
                             updated_at = c("1857-03-27","1857-03-27","1890-02-17","1890-02-17","1900-01-13","1900-01-13"))
hold.cov[[6]] = data.frame(trait_id = c(31,33,35),variable_id = c(81,81,81))

# required.covariates[[utrait[7]]]

hold.trait[[7]] = data.frame(trait_id = c(37,38,39,40,41,42),variable_id = c(316,316,316,316,316,316),
                             updated_at = c("1857-03-27","1857-03-27","1890-02-17","1890-02-17","1900-01-13","1900-01-13"))
hold.cov[[7]] = data.frame(trait_id = c(37,39,41),variable_id = c(81,81,81))

# required.covariates[[utrait[8]]]

hold.trait[[8]] = data.frame(trait_id = c(43,44,45,46,47,48),variable_id = c(311,311,311,311,311,311),
                             updated_at = c("1857-03-27","1857-03-27","1890-02-17","1890-02-17","1900-01-13","1900-01-13"))
hold.cov[[8]] = data.frame(trait_id = c(43,45,47),variable_id = c(81,81,81))

# required.covariates[[utrait[9]]]

hold.trait[[9]] = data.frame(trait_id = c(49,50,51,52,53,54,55,56,57),variable_id = c(4,4,4,4,4,4,4,4,4),
                             updated_at = c("1857-03-27","1857-03-27","1857-03-27","1890-02-17","1890-02-17","1890-02-17","1900-01-13","1900-01-13","1900-01-13"))
hold.cov[[9]] = data.frame(trait_id = c(49,49,50,52,52,53,55,55,56),variable_id = c(83,81,83,83,81,83,83,81,83))

# required.covariates[[utrait[10]]]

hold.trait[[10]] = data.frame(trait_id = c(58,59,60,61,62,63,64,65,66),variable_id = c(244,244,244,244,244,244,244,244,244),
                              updated_at = c("1857-03-27","1857-03-27","1857-03-27","1890-02-17","1890-02-17","1890-02-17","1900-01-13","1900-01-13","1900-01-13"))
hold.cov[[10]] = data.frame(trait_id = c(58,58,59,61,61,62,64,64,65),variable_id = c(208,243,243,208,243,243,208,243,243))

# required.covariates[[utrait[11]]]

hold.trait[[11]] = data.frame(trait_id = c(67,68,69,70,71,72),variable_id = c(293,293,293,293,293,293),
                              updated_at = c("1857-03-27","1857-03-27","1890-02-17","1890-02-17","1900-01-13","1900-01-13"))
hold.cov[[11]] = data.frame(trait_id = c(67,69,71),variable_id = c(6,6,6))

# required.covariates[[utrait[12]]]

hold.trait[[12]] = data.frame(trait_id = c(73,74,75,76,77,78,79,80,81),variable_id = c(26,26,26,26,26,26,26,26,26),
                              updated_at = c("1857-03-27","1857-03-27","1857-03-27","1890-02-17","1890-02-17","1890-02-17","1900-01-13","1900-01-13","1900-01-13"))
hold.cov[[12]] = data.frame(trait_id = c(73,73,74,76,76,77,79,79,80),variable_id = c(318,81,81,318,81,81,318,81,81))

expect1 = c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50,50,51,53,53,54,56,56,57,59,59,60,62,62,63,
           65,65,66,68,70,72,74,75,77,78,80,81)
expect2 = c(6, 12, 18, 24, 30, 36, 42, 48, 56, 56, 57, 65, 65, 66, 72, 80, 81)
expect3 = c(4,  6, 10, 12, 16, 18, 22, 24, 28, 30, 34, 36, 40, 42, 46, 48, 53, 53,
            54, 56, 56, 57, 62, 62, 63, 65, 65, 66, 70, 72, 77, 78, 80, 81)

test.traits = do.call(rbind,hold.trait)
test.cov = do.call(rbind, hold.cov)


