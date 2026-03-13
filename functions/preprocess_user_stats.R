# December 23, 2025

# filter to user_segment = anonymous
# assign program_branch
# rename asset_name with the county or "Station Location"
# access_type: combine "grid view" and "page primer view" into "dataset view"

preprocess_user_stats <- function(dat) {

  assets <- c(
    "Station Locations", "Station Location", "Deployment Information",
    "Annapolis", "Antigonish", "Cape Breton", "Colchester", "Digby",
    "Guysborough", "Halifax", "Inverness", "Lunenburg", "Pictou", "Queens",
    "Richmond", "Shelburne", "Victoria","Yarmouth", "Inland",
    "Understanding Complex Data"
  )

  old_datasets <- c(
    "Aspotogan Harbour Daily Average Current Speed by Deployment",
    "Nova Scotia Current Speed and Direction Data"
  )

  access_views <- c(
    "grid view", "primer page view", "visualization canvas view", "story view"
  )

  dat %>%
    filter(
      user_segment == "anonymous",
      !(asset_name %in% old_datasets),
      asset_type != "filter" # this is mainly the User Stats dataset
    ) %>%
    mutate(
      asset_name = if_else(str_detect(asset_name, "Inland"), "Inland", asset_name),

      program_branch = case_when(
        str_detect(asset_name, "Water Quality Data") ~ "Water Quality",
        # old Water Quality dataset names
        str_detect(asset_name, "Coastal Monitoring Program") ~ "Water Quality",
        str_detect(asset_name, "Current") ~ "Current",
        str_detect(asset_name, "Wave") ~ "Wave",
        str_detect(asset_name, "Current and Wave") ~ "Current and Wave",

        str_detect(asset_name, "Inland") ~ "Inland",
        str_detect(asset_name, "Understanding Complex Data") ~ "Story",
        TRUE ~ NA
      ),
      asset_name = str_extract(asset_name, paste(assets, collapse = "|")),
      asset_name = case_when(
        asset_name == "Station Location" ~ "Station Locations",
        asset_name == "Understanding Complex Data" ~ "Article",
        TRUE ~ asset_name
      ),

      access_type = case_when(
        access_type %in% access_views ~ "view",
        TRUE ~ access_type
      )
    ) %>%
    select(
      program_branch,
      timestamp, value, access_type, asset_name, data_product = asset_type
    )

}



