label_leaps = function(capitalize = "none", wrap = Inf) {
  scales:::force_all(capitalize, wrap)
  function(x) {
    leap_labels <- data.frame(
      label = c(
        "High Expectations with Unlimited Opportunities",
        "Whole-Child Focus",
        "Rigorous Learning",
        "Relevance",
        "Affirmation of Self & Others",
        "Social Consciousness & Action",
        "Connection & Community",
        "Customization",
        "Active Self-Direction",
        "Anytime, Anywhere Learning"),
      leap = c(
        "leaps_high_expectations",
        "leaps_whole_child",
        "leaps_rigorous_learning",
        "leaps_relevance",
        "leaps_affirmation",
        "leaps_social_consciousness",
        "leaps_connection",
        "leaps_customization",
        "leaps_self_direction",
        "leaps_anytime_anywhere"))
    if(any(!x %in% leap_labels$leap)) warning("Missing label for Leap variable")
    labels = leap_labels$label[match(x, leap_labels$leap)]
    labels[is.na(labels)] = x[is.na(labels)]
    if(capitalize == "title") {
      labels = str_to_title(labels)
    }
    if(capitalize == "first") {
      labels = str_to_sentence(labels)
    }
    if(is.finite(wrap)) {
      labels = str_wrap(labels, width = wrap)
    }
    return(labels)
  }
}
