get_window_details = function(start, data){
    stop <- start + 7
    data$animal_id <- as.character(data$animal_id)
    interval <- data[data$DATE > start & data$DATE < stop,]
    
    old.ids <- unique(data$animal_id[data$DATE < start])
    oldneg.ids <- unique(data$animal_id[data$DATE < start & data$rvf == 0])
    oldpos.ids <- unique(data$animal_id[data$DATE < start & data$rvf == 1])
    
    interval.ids <- unique(interval$animal_id)
    positive.ids <- unique(interval$animal_id[interval$rvf == 1])
    convert.ids <- unique(positive.ids[(positive.ids %in% oldneg.ids) & 
                                           !(positive.ids %in% oldpos.ids)])
    replacement.ids <- unique(interval.ids[!(interval.ids %in% old.ids)])
    missing.ids <- old.ids[!(old.ids %in% interval.ids)]  
    
    future.ids <- unique(data$animal_id[data$DATE > stop])
    found.later.ids <- unique(missing.ids[
        !(missing.ids %in% oldpos.ids) & 
            (missing.ids %in% future.ids)])
    lost.ids <- unique(missing.ids[
        !(missing.ids %in% oldpos.ids) & 
            !(missing.ids %in% future.ids)])
    out <- c(
        num_tested = nrow(interval), 
        num_pos = sum(interval$rvf),
        num_seroconvert = length(convert.ids),
        num_new_replacement = length(replacement.ids),
        # Num missing= missing now, exist/neg in prev AND exist in future
        num_missing_found_later = length(found.later.ids),
        # lost forever=missing ids that are neg in past AND not in future
        num_lost_forever = length(lost.ids))
    return(out)
}

get_one_village = function(n){
    weeks <- seq(floor_date(min(df$DATE), "week"), 
                 ceiling_date(max(df$DATE), "week"), by = 7)
    temp <- data.frame(
        model.start = seq(0, length(weeks) - 1, 1),
        date.start = weeks,
        diptankid = rep(n, length(weeks)),
        num_tested = rep(NA, length(weeks)),
        num_pos = rep(NA, length(weeks)),
        num_seroconvert = rep(NA, length(weeks)),
        num_new_replacement = rep(NA, length(weeks)),
        num_missing_found_later = rep(NA, length(weeks)),
        num_lost_forever = rep(NA, length(weeks)))
    for(i in 1:length(temp[,1])){
        temp[i, c(4:9)] <- get_window_details(
            temp$date.start[i], subset(df, place == n))
    }
    return(temp)
}
