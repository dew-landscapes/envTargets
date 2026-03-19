
meta_run <- function(scales = "settings/scales.yaml"
                     , grep_projects = "^env"
                     , projects_dir = "../"
                     , stores_dir = "../../out"
                     ) {

  s <- yaml::read_yaml(scales)

  s <- s[grep(grep_projects, names(s))]

  parse_store_metadata(names(s)[names(s) != "envOcc"]
                       , store_base = stores_dir
                       , targets_yaml = "_targets.yaml"
                       , scales_yaml = "scales.yaml"
                       , max_depth = 6
                       )

}
