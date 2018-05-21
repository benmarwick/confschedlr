create <- function(pkg = ".", auth_token, private = FALSE, copyright_holder){
  devtools::use_github(pkg, auth_token = auth_token, protocol = "ssh", private = private)
  devtools::use_mit_license(copyright_holder)
  rrtools::use_analysis()
  rrtools::use_readme_rmd()
  rrtools::use_dockerfile()
  rrtools::use_travis()
  devtools::use_testthat()
}

create(auth_token = "xx")


mtcars %>%
  filter(mpg > 10) %>%
  select(cyl)


x <- 1:18
u <- c(3,6,12)
v <- c(5,10,15)
data1 <- data.frame(x, loc_lower = findInterval(x, u),
                    loc_upper =    findInterval(x, v + 1))
data1

write.csv(iris,
          "../data/derived_data/iris.csv")

all_nonposter_sessions %>%
  filter(`Session Id` %in% c(3115, 3147, 3175, 3116, 3152, 3162)) %>%
  distinct(`Session Id`)
  View()

  all_abstracts %>%
    group_by(`Session Type`, `First Name`, `Last Name`) %>%
    tally(sort = TRUE) %>%
    filter(grepl("Poster", `Session Type`))

# want no more than n in each group
  n <-  3
  x <- 1:13

data.frame(x = x,
           y = x %% 2)

x2 <- (split(x, ceiling(seq_along(x)/n)))
y <- unlist(map(names(split(x, ceiling(seq_along(x)/n))), ~rep(.x, n)))

data.frame(x = x,
           y = y)


y <- as.character(cut_number(x, 3))
data.frame(x = x,
           y = y)

chunk <- function(x,n)
{
  f <- sort(rep(1:(trunc(length(x)/n)+1),n))[1:length(x)]
  return(split(x,f))
}

y <- chunk(x, 3)
names(y)
length(y[[1]])
map(y, ~rep(x, length(.x)))

sapply(y, )



z7  <-
  all_nonposter_sessions_with_session_durations  %>%
  filter(`Session Id` %in% c("3115", "3147", "3175", "3116", "3152", "3162"))

xx <- c(1:5, 1:5, 1:5)
rle(xx)$lengths


df <- data.frame(country = rep(c("A","B"), each=5), year=rep(2000:2004, times=2), event=c(0,0,1,0,0,1,0,0,1,0), stringsAsFactors=FALSE)

df <- data.frame(xx = c(1:5, 1:5, 1:5))

df %>%
group_by(xx, idx = cumsum(xx == 1L))

#-------------------------------------------------------------
# prepare data for pkg fn
cols_to_keep <- c("Abstract Id"  ,        "First Name"     ,      "Last Name"
, "CoAuthor1 First Name", "CoAuthor1 Last Name" , "CoAuthor1 Employer"
, "CoAuthor2 First Name" ,"CoAuthor2 Last Name" , "CoAuthor2 Employer"
, "CoAuthor3 First Name" ,"CoAuthor3 Last Name" , "CoAuthor3 Employer"
, "CoAuthor4 First Name", "CoAuthor4 Last Name" , "CoAuthor4 Employer"
, "Title"       ,         "Geographic Focus" ,    "Keyword1"
, "Keyword2"     ,        "Keyword3"        ,     "Abstract"
, "Session Id"    ,       "Session Type"        , "Role")

all_papers <-
  all_nonposter_sessions_with_session_durations %>%
  select(cols_to_keep) %>%
  mutate(session_id = `Session Id`,
         first_name = `First Name`,
         last_name = `Last Name`) %>%
  select(-`First Name`, -`Last Name`)

write_csv(all_papers,
          "C:/Users/bmarwick/Desktop/saaabstracts/tests/testthat/all_papers.csv")

all_sessions <-
  org_and_gen_session_durations %>%
  mutate(session_id = `Session Id`,
         session_duration = TIME) %>%
  select(session_id, session_duration)

write_csv(all_sessions,
          "C:/Users/bmarwick/Desktop/saaabstracts/tests/testthat/all_sessions.csv")

total_capacity <-
  structure(
    list(
      day_part = c(
        "Th-morning",
        "Th-afternoon",
        "Th-evening",
        "Fr-morning",
        "Fr-afternoon",
        "Sa-morning",
        "Sa-afternoon",
        "Su-morning"
      ),
      rooms_available = c(36, 35, 36, 31, 32, 30, 30, 28),
      hours_available = c(144,
                         140, 144, 124, 128, 120, 120, 112)
    ),
    class = c("tbl_df", "tbl",
              "data.frame"),
    row.names = c(NA, -8L),
    .Names = c("day_part",
               "rooms_available", "hours_available")
  )

write_csv(total_capacity,
          "C:/Users/bmarwick/Desktop/saaabstracts/tests/testthat/total_capacity.csv")


setwd("C:/Users/bmarwick/Desktop/saaabstracts/tests/testthat/")
library(readr)
all_papers <- read_csv("all_papers.csv")
all_sessions <- read_csv("all_sessions.csv")
total_capacity <- read_csv("total_capacity.csv")

testing <-
  fill_day_parts_with_sessions(total_capacity,
                             all_papers,
                             all_sessions,
                             buffer = 20,
                             verbose_output = FALSE)
testing2 <-
  fill_slot_with_sessions(total_capacity,
                          testing$all_papers_with_blocks,
                          the_day_part = 'Th-morning')

testing$total_capacity

testing$all_papers_with_blocks %>%
  distinct(session_id) %>%
  nrow

all_papers %>%
  distinct(session_id) %>%
  nrow

all_nonposter_sessions_with_session_durations %>%
  mutate(session_id = `Session Id`) %>%
  distinct(session_id) %>%
  nrow




# check to see what we got from the loop...# check to see what we got from the loop...
sessions_per_day <-
  all_of_the_day_parts %>%
  group_by(day_part_from_while) %>%
  summarise(n_sessions = length(unique(`Session Id`)))

hours_per_day <-
  all_of_the_day_parts %>%
  group_by(day_part_from_while, `Session Id`) %>%
  slice(1) %>%
  summarise(n_hours = sum(session_time_allowed)) %>%
  ungroup() %>%
  group_by(day_part_from_while) %>%
  summarise(n_hours = sum(n_hours))


sessions_per_day <-
  testing$all_papers_with_blocks %>%
  group_by(day_part_from_while) %>%
  summarise(n_sessions = length(unique(session_id)))

hours_per_day_t <-
  testing$all_papers_with_blocks %>%
  group_by(day_part_from_while, session_id) %>%
  slice(1) %>%
  summarise(n_hours = sum(session_duration)) %>%
  ungroup() %>%
  group_by(day_part_from_while) %>%
  summarise(n_hours = sum(n_hours))

# a 2-dimensional example
x <- rbind(matrix(rnorm(100, sd = 0.3), ncol = 2),
           matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2))
colnames(x) <- c("x", "y")
x <- x[,1]
(cl <- kmeans(x, 2))
plot(x, , col = cl$cluster)
