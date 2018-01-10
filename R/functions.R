

#' fill_day_parts_with_sessions
#'
#' @description The purpose of this function is to assign papers into time blocks of a conference schedule in a way that avoids scheduling the same person in more than one time block at the same time.
#'
#' @param total_capacity Data frame with one columns of day-parts (day_part, e.g. Thursday morning) and another column of total amount of hours to fill in each day-part (total_hours)
#' @param all_papers Data frame of submitted papers, one row per paper. Each paper must already be allocated to a session, and have a value in a session_id column
#' @param all_sessions Data frame of sessions, one row per session. Each session must have a value in a session_id column, and each session must have a value for the session_duraction' column
#' @param buffer integer A number of papers to avoid overfilling each day-part. Vary this number to experiment with optimising how efficiently each block of time is filled.
#' @param verbose_output logical, Show detailed output while the function is running? May be useful for trouble-shooting.
#' @import dplyr
#'
#' @return A list with two data frames
#' @export
#'
#' @examples
#'
#' output <-
#' fill_day_parts_with_sessions(total_capacity,
#'                              all_papers,
#'                              all_sessions)
#'
#'
fill_day_parts_with_sessions <- function(total_capacity,
                                         all_papers,
                                         all_sessions,
                                         buffer = 30,
                                         verbose_output = TRUE){

  # check if we have a session_id column
  if (length(all_papers$session_id) == 0) stop("There is no column 'session_id' in the data frame of conference papers. Please add this column and try again.")

  # check to see if every paper has been assigned to a session
  number_of_papers_with_session_id <- sum(!is.na(all_papers$session_id))
  total_number_of_papers <-  nrow(all_papers)
  number_of_papers_without_session_id <-
    total_number_of_papers - number_of_papers_with_session_id
  if (number_of_papers_with_session_id == 0) stop(glue('{number_of_papers_without_session_id} papers do not have a session ID. Please ensure that all papers are assigned to a session and try again.'))

  # check to see if we have session durations
  if (length(all_sessions$session_duration) == 0) stop("There is no column 'session_duration' in the data frame of conference sessions. Please add this column and try again.")

  # check to see if we have a first name and last name for each paper
  if (length(all_papers$first_name) == 0) stop("There is no column 'first_name' in the data frame of conference papers. Please add this column and try again.")

  if (length(all_papers$last_name) == 0) stop("There is no column 'last_name' in the data frame of conference papers. Please add this column and try again.")

  # make uid of name
  all_papers <-
    all_papers %>%
    mutate(uid = glue('{first_name}_{last_name}'))

  # we need the session duration in the with each paper
  all_papers <-
    all_papers %>%
    left_join(all_sessions)


  all_nonposter_sessions_with_session_durations <- all_papers
  all_nonposter_sessions_with_session_durations$session_time_allowed <-
    all_nonposter_sessions_with_session_durations$session_duration
  # prepare data set to remove rows from
  z7 <- all_sessions

  total_cap <- total_capacity

  all_of_the_day_parts <- NULL
  total_cap$hours_of_sessions <- 0
  total_cap$n_sessions <- 0
  total_cap$c_sessions <- 0
  container_for_this_day_part_with_day_from_while <- 0
  i <- 1

  # start with each day-part...
  for (i in seq_len(nrow(total_cap))) {
    # stop loop gracefully when we have assigned all people to day-parts
    if (nrow(z7) == 0) {
      break
    }

    # how many rows (people) to we have remaning to allocate to a day-part?
    if (verbose_output) print(glue('number of people remaining to classify is {nrow(z7)}'))

    # add some buffer time to nudge it back so we don't overfill a day-part
    # maybe if this buffer is bigger, the knapsack will be faster?
    # A big enough buffer should avoid the 0 group in the knapsack results
    # which indicates that the knapsack fn can't fit all the sessions in the
    # day-part

    target_capacity <- total_cap$hours_available[i] - buffer

    # prepare a while statement that will take a session,
    # check to see if there is a name clash with sessions,
    # skip adding it if there is clash, then check to see
    # the cumulative duration of the sessions in the day-part
    # has exceeded the total duration of the day-part,
    # and stop adding sessions if it has.

    # make sure some variables are empty
    cumulative_duration_of_sessions   <-  0
    container_for_this_day_part <- 0
    all_the_people_in_the_session <- 0
    j <- 1

    while (cumulative_duration_of_sessions <= target_capacity &
           j <= nrow(z7)) {
      # randomly reorder the rows to see if we can solve a problem
      # z7 <- z7[sample(nrow(z7)),]
      get_a_session <- z7[j,]

      # get the names of all the people in this session...
      all_the_people_in_this_session <-
        all_nonposter_sessions_with_session_durations %>%
        filter(session_id == get_a_session$session_id)

      # get names from what we already have added to this day-part
      if (j == 1) {
        # do nothing is this is the first incoming session
        distinct_names_for_each_session_so_far <- 0
      } else {
        # get distinct names for each session previously added
        # to this day-part
        # (don't care about same name multiple times in one session)
        distinct_names_for_each_session_so_far <-
          container_for_this_day_part %>%
          group_by(uid) %>%
          distinct(uid, .keep_all = TRUE) %>%
          pull(uid)
      }

      # compare names already in here to names we are about to add
      if (j == 1) {
        # do nothing
        names_already_in <- 0
      } else {
        # check to see if incoming session has names that already exist in this day_part
        names_already_in <-
          sum(
            all_the_people_in_this_session$uid %in% distinct_names_for_each_session_so_far
          )
      }

      # accumulate the incoming session onto the sessions
      # we've already added to this day-part, if there are no
      # name conflicts
      if (j == 1) {
        container_for_this_day_part <-
          all_the_people_in_this_session
      } else {
        if (names_already_in == 0) {
          # if there are no name clashes,
          # add these names to previous names in this day-part

          # bind rows of session that has no clashing  names with
          # sessions we already added to this day-part
          container_for_this_day_part <-
            bind_rows(all_the_people_in_this_session,
                      container_for_this_day_part)
        }  else {
          # do not add this session to the day-part
        }
      }

      # compute the duration of session that we've added so far to
      # this day-part
      cumulative_duration_of_sessions <-
        container_for_this_day_part %>%
        distinct(session_id, .keep_all = TRUE) %>%
        pull(session_time_allowed) %>%
        sum

      # store this result
      total_cap$hours_of_sessions[i] <-
        cumulative_duration_of_sessions

      # show a check of what's happening
      if (j == 1) {
        progress_check <-
          data_frame(
            i = i,
            j = j,
            'cumulative duration' = cumulative_duration_of_sessions,
            'sessions added' =  length(unique(
              container_for_this_day_part$session_id
            )),
            people =  nrow(container_for_this_day_part)
          )
      } else {
        progress_check <- bind_rows(
          progress_check,
          data_frame(
            i = i,
            j = j,
            'cumulative duration' = cumulative_duration_of_sessions,
            'sessions added' =  length(unique(
              container_for_this_day_part$session_id
            )),
            people =  nrow(container_for_this_day_part)
          )
        )
      }

      if (verbose_output) print(progress_check)

      # move to next session
      j <- j + 1
    }
    # end of the while loop

    # add an identifier so we can see where we have assigned each session so far
    container_for_this_day_part_with_day_from_while <-
      container_for_this_day_part %>%
      mutate(day_part_from_while  = as.character(total_cap$day_part[i]))

    # bind rows to previous day-parts we might have done
    if (i == 1) {
      all_of_the_day_parts <-
        container_for_this_day_part_with_day_from_while

    } else {
      # check to see

      all_of_the_day_parts <-
        bind_rows(container_for_this_day_part_with_day_from_while,
                  all_of_the_day_parts)
    }

    # # progress check
    if (verbose_output) print(glue('i = {i}'))
    #
    if (verbose_output) print(
      all_of_the_day_parts %>%
        group_by(day_part_from_while) %>%
        summarise(papers = n(),
                  sessions = length(unique(session_id)))
    )

    total_cap$n_sessions[i] <-
      length(unique(
        container_for_this_day_part_with_day_from_while$session_id
      ))

    total_cap$c_sessions[i] <-  sum(total_cap$n_sessions)

    if (verbose_output) print(total_cap)

    # for the next day-part, we need to make sure
    # we don't add the same session twice to different day-parts
    # so let's update our big table of abstracts, and drop the session that
    # we've already got in the day-part

    z7 <-
      z7 %>%
      filter(!session_id %in% unique(container_for_this_day_part$session_id))

  }

  # how many sessions were not allocated?
  n_not_allocated <-
    length(unique(all_sessions$session_id)) - length(unique(all_of_the_day_parts$session_id))

  which_session <-
    setdiff(unique(all_sessions$session_id),
            unique(all_of_the_day_parts$session_id))

  if (n_not_allocated != 0) message(glue('{n_not_allocated} sessions ({which_session}) were not allocated. Try lowering the buffer value and try again.'))

  return(
    list(
      total_capacity = total_cap,
      all_papers_with_blocks = all_of_the_day_parts
    )
  )

}



#' fill_slot_with_sessions
#'
#' @param total_capacity Data frame of day-parts (e.g. Thursday morning) and total amount of hours to fill in each day-part. Must contain a column 'day_part' that holds the names of of the day-parts (e.g. "Thu-morning").
#' @param day_parts_with_sessions Data frame output by the fill_day_parts_with_sessions() function. Must contain a column day_part_from_while that holds the value of the day-part that was computed by fill_day_parts_with_sessions(), and a column session_id that contains the session ID.
#' @param day_part character, the specific day-part that we are aiming to fill
#' @param slot_duration integer, the duration of this individual slots in this day-part, in hours
#' @importFrom adagio mknapsack
#'
#' @return A data frame
#' @export
#'
#' @examples
#'
#' output_for_Th_morning <-
#' fill_slot_with_sessions(total_capacity,
#'                         output$all_papers_with_blocks,
#'                         the_day_part = 'Th-morning')
#'
fill_slot_with_sessions <- function(total_capacity,
                                    all_papers_with_blocks,
                                    the_day_part,
                                    slot_duration = 4){

  # get the slots available for this day-part
  the_slots <-
    total_capacity %>%
    filter(day_part == the_day_part) %>%
    pull(rooms_available)

  # get the session id's assigned to this day-part
  the_sessions <-
    all_papers_with_blocks %>%
    group_by(session_id) %>%
    filter(day_part_from_while == the_day_part) %>%
    slice(1)

  slots_to_fill <- rep(slot_duration, the_slots) * 60 # so many n hour slots that we need to fill

  sessions_we_have <- the_sessions$session_time_allowed * 60

  # mknapsack calling signature is: mknapsack(values, weights, capacities)
  solution <- mknapsack(sessions_we_have,
                        sessions_we_have,
                        slots_to_fill)

  the_sessions$computed_slot <- solution$ksack

  return(the_sessions)
}
