add_cohort_overview <-
    function(selected_measures,
             module_name,
             profile_color,
             profile_color_type,
             profile_variable_list) {
        r2d3::r2d3(
            data = selected_measures,
            script = app_sys("app/build/cohort_overview.js"),
            d3_version = 5,
            dependencies = app_sys("app/build/d3-legend.js"),
            options = list(
                id = module_name,
                color = profile_color,
                color_type = profile_color_type,
                columns = profile_variable_list
            )
        )
    }