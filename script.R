library(tidyverse)
library(here)
library(readr)
library(hrbrthemes)

# ratings for all the fillers
d <- list.files(
    path = "results/",
    pattern = "*.csv",
    full.names = T
) %>%
    map_df(~ read_csv(., col_types = cols(.default = "c"))) %>%
    drop_na() %>%
    mutate(
        kind = factor(kind),
        judgment = as.integer(judgment),
        # scale values from 0 to 1
        judgment = (judgment - min(judgment)) / (max(judgment) - min(judgment))
    )

# combine item texts and ratings
items <- read.table(
    here("items", "fillers.txt"),
    sep = "\t",
    quote = "",
    col.names = c("kind", "item", "cond", "text")
) %>%
    mutate(item = as.character(item)) %>%
    right_join(
        d %>%
            group_by(kind, item) %>%
            summarise(
                mean = round(mean(judgment, na.rm = TRUE), 2),
                se = round(sciplot::se(judgment, na.rm = TRUE), 2),
                n = n()
            )
    ) %>%
    write_delim(here("items", "fillers.csv"), delim = "\t", quote = "none")


## %######################################################%##
####                data visualization                  ####
## %######################################################%##

source(here("assets", "theme.R"))

p_means <- d %>%
    group_by(kind) %>%
    mutate(
        kind = case_when(
            kind == "ambig_pronoun" ~ "Ambiguous pronoun",
            kind == "attachment_ambig" ~ "Attachment ambiguity",
            kind == "belief_bedauern" ~ "Wrong belief, *bedauern*",
            kind == "belief_erinnern" ~ "Wrong belief, *erinnern*",
            kind == "belief_froh" ~ "Wrong belief, *froh*",
            kind == "idiom_scrambled" ~ "Idiom, scrambled word order",
            kind == "idiom_unmarked" ~ "Idiom, umarked word order",
            kind == "none" ~ "Fully grammatical",
            kind == "overinformative" ~ "Overinformative answer",
            kind == "partial_answer" ~ "Underinformative answer",
            kind == "praet" ~ "Non-standard preterite form",
            kind == "sem" ~ "Semantic violation",
            kind == "strict_sloppy" ~ "VP ellipsis, strict vs. sloppy",
            kind == "stupid" ~ "Nonsense",
            kind == "syn" ~ "Syntactic violation",
            kind == "synsem" ~ "Syntactic and semantic violation",
            kind == "tun" ~ "*tun* periphrasis",
            TRUE ~ "<span style='color:red;'>**YOU FORGOT SOME**</span>"
        ),
        kind = glue::glue("{kind} <span style='color:gray;'>({n()})</span>"),
    ) %>%
    ungroup() %>%
    mutate(
        kind = fct_reorder(kind, judgment, .fun = mean)
    ) %>%
    ggplot(aes(
        x = kind, y = judgment, color = presentation
    )) +
    stat_summary(
        fun.data = mean_cl_normal,
        geom = "errorbar",
        width = .25,
        size = 1.1,
        alpha = .5
    ) +
    stat_summary(fun = mean, geom = "point", size = 4) +
    facet_wrap(
        ~ presentation,
        strip.position = "bottom",
        labeller = as_labeller(
            c(
                "audio" = "Auditory presentation",
                "text" = "Written presentation"
            )
        )
        ) +
    guides(color = "none") +
    labs(
        y = "Normalized mean judgments \u00B1 95% standard CI",
        x = "Filler type <span style='color:gray;'>(*N*)</span>",
        caption = "Visualization by Maik Thalmann,
        <span style='color:#066b8a;font-family: monospace, monospace;'>
        github.com/mkthalmann
        </span>"
    ) +
    coord_flip(clip = "off") +
    scale_y_continuous(labels = c("0", "¼", "½", "¾", "1")) +
    scale_color_manual(values = c("#066b8a", "#8a064a")) +
    theme(axis.text.y = element_markdown(hjust = 0))

# export plots
ggsave(
    here("images", "filler_means.pdf"),
    p_means,
    device = cairo_pdf,
    width = 18,
    height = 8,
    units = "in",
)
