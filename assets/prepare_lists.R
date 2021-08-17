library(tidyverse)
library(here)
library(readr)

d_bench <- read.csv(here("results", "original", "exprag_bench.csv")) %>%
    rename(kind = viol) %>%
    mutate(
        id = as.character(id),
        presentation = "text"
    )

d_gehen <- read.csv(here("results", "original", "gehen_semantics.csv")) %>%
    filter(group == "young") %>%
    select(-age, -group) %>%
    relocate(id, item, kind, judgment) %>%
    mutate(
        presentation = "text"
    )

d_psp <- read.csv(here("results", "original", "psp_semantics.csv")) %>%
    select(-gender) %>%
    mutate(
        kind = ifelse(grepl("PA", item), "partial_answer", "overinformative"),
        item = gsub("PA|Kon", "", item),
        item = as.integer(item)
    ) %>%
    relocate(id, item, kind, judgment) %>%
    mutate(
        presentation = "audio"
    )

d_evelyn <- read.csv(
    here("results", "original", "evelyn_results.csv"),
    sep = ";"
) %>%
    separate(item_id, into = c("kind", "item", "cond"), sep = "_") %>%
    mutate(
        kind = gsub("<id:0", "", kind),
        cond = gsub(">", "", cond),
        kind = case_when(
            kind == "3" & cond == "a" ~ "idiom_unmarked",
            kind == "3" & cond == "b" ~ "idiom_scrambled",
            kind == "4" & cond == "a" ~ "belief_froh",
            kind == "4" & cond == "b" ~ "belief_erinnern",
            kind == "4" & cond == "c" ~ "belief_bedauern",
            kind == "9" & cond == "a" ~ "none",
            kind == "9" & cond == "b" ~ "syn",
            kind == "9" & cond == "c" ~ "sem",
            kind == "9" & cond == "d" ~ "synsem",
            TRUE ~ "other"
        ),
        vp_id = as.factor(vp_id),
        id = as.character(as.numeric(vp_id)),
        presentation = "text",
        item = as.numeric(item)
    ) %>%
    filter(kind != "other") %>%
    select(id, item, kind, rating, presentation) %>%
    rename(judgment = rating) %>%
    as_tibble()

# add benchmarking items from evelyn's exp to the other bench items
d_bench <- d_bench %>%
    bind_rows(
        d_evelyn %>%
        filter(kind %in% c("none", "syn", "sem", "synsem"))
    )

# remove benchmarking items
d_evelyn <- d_evelyn %>% filter(!kind %in% c("none", "syn", "sem", "synsem"))


d_scram <- read.table(here("results", "original", "scrambling_syntax.txt"), sep = "|", header = T) %>%
    filter(Subexp %in% c(6, 7, 8, 9)) %>%
    rename(id = Subject, item = Item, judgment = Judgment) %>%
    mutate(
        kind = case_when(
            Subexp == 6 ~ "attachment_ambig",
            Subexp == 7 ~ "ambig_pronoun",
            Subexp == 8 ~ "strict_sloppy",
            Subexp == 9 ~ "stupid"
        ),
        id = as.character(id)
    ) %>%
    select(id, item, kind, judgment) %>%
    mutate(
        presentation = "text"
    )

d_scram_audio <- read_csv(here("results", "original", "scram_audio.csv")) %>%
    mutate(
        kind = case_when(
            kind == 6 ~ "attachment_ambig",
            kind == 7 ~ "ambig_pronoun",
            kind == 8 ~ "strict_sloppy",
            kind == 9 ~ "stupid"
        ),
        id = as.character(id),
        presentation = "audio"
    ) %>%
    relocate(id, item, kind, judgment, presentation)

d_all <- d_bench %>%
    bind_rows(d_gehen) %>%
    bind_rows(d_psp) %>%
    bind_rows(d_scram) %>%
    bind_rows(d_scram_audio) %>%
    bind_rows(d_evelyn)

d_all %>%
    group_by(kind, presentation) %>%
    mutate(
        participants = length(unique(id)),
        items = max(item)
    ) %>%
    group_by(kind) %>%
    group_walk(
        ~ write_csv(.x, here("results", paste0(.y$kind, "_results.csv"))),
        .keep = TRUE
    )

# note that overinformative and partial_answer were done with scales from 1 to 5
# instead of 1 to 7
d_all %>%
    group_by(kind) %>%
    summarise(
        max = max(judgment),
        min = min(judgment)
    )
