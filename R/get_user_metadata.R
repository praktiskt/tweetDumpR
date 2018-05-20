#' @title Get Twitter profile data.
#' @description This function retrieves a Twitter users metadata, including number of followers, bio, join date and more.
#' @export
#' @param username The name of the twitter user you wish to lookup
#' @return Returns a tibble containing followers
#' @examples
#' get_user_metadata(username = 'hadleywickham')
get_user_metadata = function(username) {
  
  # Libs.
  suppressMessages({
    require(httr)
    require(rvest)
    require(dplyr)
    require(magrittr)
    require(tidyr)
    require(tibble)
    require(xml2)
  })
  
  # Url to target.
  url = stringr::str_interp('https://twitter.com/${username}?lang=en')
  
  # Download and parse user profile.
  get = GET(
    url = url,
    add_headers(
      "Accept" = 'application/json, text/javascript, */*; q=0.01',
      "Referer" = stringr::str_interp('https://twitter.com/${username}'),
      "User-Agent" = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_6) AppleWebKit/603.3.8 (KHTML, like Gecko) Version/10.1.2 Safari/603.3.8',
      "X-Twitter-Active-User" = 'yes',
      "X-Requested-With" = 'XMLHttpRequest'
    )
  ) %>%
    extract2("content") %>%
    rawToChar() %>% 
    read_html()
  
  # Extract things!
  no_tweets = get %>% 
    html_nodes(".ProfileNav-item--tweets") %>% 
    html_nodes(".ProfileNav-value") %>% 
    html_attr("data-count") %>% 
    as.numeric() %>% 
    ifelse(length(.)==0, 0, .)
    
  no_following = get %>% 
    html_nodes(".ProfileNav-item--following") %>% 
    html_node(".ProfileNav-value") %>% 
    html_attr("data-count") %>% 
    as.numeric() %>% 
    ifelse(length(.)==0, 0, .)
  
  no_followers = get %>% 
    html_nodes(".ProfileNav-item--followers") %>% 
    html_node(".ProfileNav-value") %>% 
    html_attr("data-count") %>% 
    as.numeric() %>% 
    ifelse(length(.)==0, 0, .)
  
  no_likes = get %>% 
    html_nodes(".ProfileNav-item--favorites") %>% 
    html_node(".ProfileNav-value") %>% 
    html_attr("data-count") %>% 
    as.numeric() %>% 
    ifelse(length(.)==0, 0, .)
  
  no_photos_videos = get %>% 
    html_nodes(".PhotoRail-headingWithCount") %>% 
    html_text() %>% 
    stringr::str_trim("both") %>% 
    stringr::str_replace(" Photos and videos", "") %>% 
    stringr::str_replace(",", "") %>% 
    as.numeric() %>% 
    ifelse(is.na(.), 0, .)
  
  profile_name = get %>% 
    html_nodes(".ProfileHeaderCard-name") %>% 
    html_nodes(".ProfileHeaderCard-nameLink") %>% 
    html_text()
  
  screen_name = get %>% 
    html_nodes(".ProfileHeaderCard-screennameLink") %>% 
    html_nodes(".u-linkComplex-target") %>% 
    html_text()
  
  bio = get %>% 
    html_nodes(".ProfileHeaderCard-bio") %>% 
    html_text()
  
  location_text = get %>% 
    html_nodes(".ProfileHeaderCard-locationText") %>% 
    html_text() %>% 
    stringr::str_trim("both")
  
  profile_header_url = get %>% 
    html_nodes(".ProfileHeaderCard-urlText") %>% 
    html_text() %>% 
    stringr::str_trim("both")
  
  # Messy date format, clean up and store properly formatted.
  join_date = get %>%
    html_nodes(".ProfileHeaderCard-joinDateText") %>%
    html_attr("title") %>%
    stringr::str_replace("\\.", "") %>%
    stringr::str_replace(" -", "") %>%
    stringr::str_split_fixed(" ", 5) %>%
    as.tibble() %>%
    left_join(tibble(label = month.abb) %>%
                mutate(number = sprintf("%02d", row_number())),
              by = c("V4" = "label")) %>%
    mutate(V4 = number,
           dttm = paste0(V5, "-", V4, "-", V3, " ", V1, " ", V2)) %>%
    magrittr::extract2("dttm") %>%
    as.POSIXct(format = "%Y-%m-%d %I:%M %p", tz = "UTC")
  
  is_verified = get %>% 
    html_nodes(".ProfileHeaderCard") %>% 
    html_nodes(".Icon--verified") %>% 
    length() %>% 
    equals(., 0) %>% 
    not()
  
  user_id = get %>% 
    html_nodes(".ProfileNav") %>% 
    html_attr("data-user-id") %>% 
    as.numeric()
    
  return(
    tibble(
      user_id = user_id, 
      screen_name = screen_name,
      profile_name = profile_name, 
      join_date = join_date, 
      is_verified = is_verified,
      bio = bio, 
      location_text = location_text, 
      no_tweets = no_tweets, 
      no_followers = no_followers, 
      no_following = no_following,
      no_likes = no_likes, 
      no_photos_videos = no_photos_videos    
    )
  )
}
