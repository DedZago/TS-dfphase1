library(devtools)
library(usethis)

load_all("dfphase1")

Student = dfphase1::Student
mphase1(x = Student, plot = TRUE, post.signal = TRUE, isolated = TRUE, step = TRUE, indep=FALSE, alpha = 0.05, gamma = 0.5, K = 7, lmin = 5, L = 1000, seed = 11642257) 
