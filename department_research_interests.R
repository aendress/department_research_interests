## ----extract-code, eval = FALSE----------------------------------------------------------------------------
# # Extract code to run on the command line, which is much faster than in the GUI
# 
# knitr::purl("interests.Rmd", output = "interests.R")
# 


## ----setup, include=FALSE----------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


library(httr)
library(tidyverse)
library(xlsx)

DEBUG_FLAG <- FALSE
RECALCULATE_EVERYTHING <- TRUE
#MODEL <- "mistral"
MODEL <- "llama3.2"


## ----define-ollama-request-function------------------------------------------------------------------------

query_ollama <- function(prompt, system_prompt = NULL, model = "llama3.2", temperature = .6, top_k = 15, top_p = .35, num_ctx = 8192, verbose = FALSE){
    
    # Initialize the payload with model and options
    payload <- list(
        model = model,
        stream = FALSE,
        options = list(
            temperature = temperature, # The temperature of the model. Increasing the temperature will make the model answer more creatively. (Default in ollama: 0.8)
            top_k = top_k, # Reduces the probability of generating nonsense. A higher value (e.g. 100) will give more diverse answers, while a lower value (e.g. 10) will be more conservative. (Default in ollama: 40)
            top_p = top_p,	# Works together with top-k. A higher value (e.g., 0.95) will lead to more diverse text, while a lower value (e.g., 0.5) will generate more focused and conservative text. (Default in ollama: 0.9) 
            
            num_ctx = num_ctx)
        #prompt = prompt
    )
    
    # Determine the appropriate URL based on the presence of a system prompt
    if (is.null(system_prompt)) {
        url <- "http://localhost:11434/api/generate"
    } else {
        url <- "http://localhost:11434/api/chat"
    }
    
    
    # Construct the messages list if system_prompt is provided
    if (!is.null(system_prompt)) {
        
        messages <- list(
            list(role = "system", content = system_prompt),
            list(role = "user", content = prompt)
        )
        payload$messages <- messages
        
    } else {
        
        payload$prompt <- prompt
    }
    
    
    # Send the POST request
    response <- httr::POST(
        url,
        body = payload,
        encode = "json",
        httr::content_type("application/json") # Ensures JSON content type
    )
    
    # Check the response
    if (httr::status_code(response) == 200) {
        result <- httr::content(response, as = "parsed")
    } else {
        stop("Failed to fetch response. Status code: ", httr::status_code(response))
    }
    
    if(verbose){
        return(result)
    }else{
        if (is.null(system_prompt)) {
            return(result$response)
        } else {
            return(result$message$content)
        }
    }
}

#query_ollama("What is your story? Do you like actual llama and do you spit? Answer in rhymes")


## ----define-system-prompts---------------------------------------------------------------------------------

l.system.prompts <- list(
    
    funders = str_c(
        "Please suggest 5 possible funding sources for the research of a UK-based academic described below, based on their research topics and research questions.",
        "For each funding source, briefly describe the kind for research that is relevant for the funder in less than 20 words, and rate the likelihood that it is relevant to the academic’s research on a scale from 1 (possibly relevant) to 5 (definitely relevant). ",
        "Strictly adhere to the following output format so it can be processed automatically:",
        "[funding source name and type of research that this source might fund]::[relevance rating]",
        "Example:",
        "National Institute for Health Research, for xamining the impact of social media use on hiring fairness and inclusivity::2",                       
        "",
        "Further intructions",
        "- The response must be in the exact format provided, no additions, deviations or any other text are allowed. Any changes to the format make the reply unusable",
        "- Use '::' as the separator (not ':'). ",
        "- Do not number the funding sources. Simply provide 5 distinct funding sources.",
        sep = "\n"),
    
    impact = str_c(
        "Please describe a possible road towards achieving impact, as defined by the UK Research Excellence Framework (REF), for the research of a UK-based academic described below, based on their research topics and research questions. The academic works in a psychology and neuroscience department.",
        "Impact, under the REF, refers to the effect on, change, or benefit to the economy, society, culture, public policy or services, health, the environment, or quality of life, beyond academia.",
        "Your response must address the following points in the exact format provided, without additional text or commentary:",
        "- Potential impact: <Describe the specific, concrete potential impact of the research, detailing how its findings or methodologies lead directly to real-world benefits, such as for individuals, communities, industries, or policies.>",
        "- Affected population: <Estimate the number or type of people/entities who might be affected by the impact.>",
        "- Steps to realize impact: <List the specific actions the academic needs to take to achieve the impact, assuming only basic research has been done so far.>",
        "- Evidence needed: <Specify the kind of evidence required to demonstrate the impact.>",
        "- REF rating: <Four star / Three star / Two star / One star>",
        "",
        "Example response:",
        "- Potential Impact: The research identified specific patterns of neural connectivity associated with early-stage Alzheimer's disease using functional MRI (fMRI). These findings might enable the development of a screening protocol for GPs to refer at-risk patients for further testing, leading to earlier diagnosis and better-targeted interventions.",
        "- Affected population: Approximately 150,000 individuals in the UK diagnosed with early-stage Alzheimer's annually, along with their caregivers.",
        "- Steps to realize impact: Partner with imaging centers to refine the protocol, train GPs on recognizing the risk factors identified by the research, and pilot the referral system in collaboration with NHS memory clinics.",
        "- Evidence needed: Data showing increased early diagnoses following protocol implementation, clinical trials demonstrating improved outcomes due to early interventions, and feedback from GPs on usability of the protocol.",
        "- REF rating: Four star",
        sep = "\n"),
    
    skills = str_c(
        "Consider the following pair of academics in a UK psychology department. Academic 1 desires certain skills in a collaborator. Please assess the likelihood that Academic 2 possesses each of the desired skills, based on either their shareable skills. Consider that Academic 2 does not have any relevant skills unless the skills desired by Academic 1 are explicitly mentioned either as Academic 2's shareable skills or as their research topics. Skills that not explicitly desired by Academic 1 are irrelevant; skills that are not explicitly offered by Academic 2 are not available. ",
        "",
        "Your response must strictly adhere to the following format (no exceptions allowed):",
        "[Skill requested by Academic 1]::[Numeric likelihood rating that Academic 2 has it on a scale from 1 (very unlikely) to 5 (very likely)]",
        "",
        "Example output:",
        "NLP::4",
        "",
        "Further instructions:",
        "- Do not include any additional text, commentary, or justification. Any deviation from this format will render the response unusable.",
        "- Provide a separate line for each skill listed under 'Desired Skills'.",
        "- If a skill cannot be assessed, write NA instead of a numeric rating, e.g., 'Machine learning::NA'.",
        sep = "\n"),
    
    collaborations = str_c(
        "Consider the following pair of academics in a UK psychology department. Based on their research topics, questions, and both the shareable and desired skills of each academic, please suggest up to three potential collaboration topics. For each topic, also suggest a relevant funder for the research. For each topic, assess the likelihood that both academics will be excited about the collaboration on a scale from 1 (very unlikely) to 5 (very likely). Ensure that each topic includes a clear rationale explaining its scientific importance and alignment with both academics' expertise and skills. The rationale should be integrated within the topic description and should not exceed 250 words.",
        "",
        "Your response must strictly adhere to the following format (no exceptions allowed):",
        "[Topic and rationale as specified above]::[Potential funder]::[Numeric likelihood rating that both academics will be excited about the collaboration on a scale from 1 (very unlikely) to 5 (very likely)]",
        "",
        "Example:",
        "Investigating the interplay between statistical learning mechanisms in language acquisition and the development of ideological polarization. This research aims to explore how innate cognitive processes involved in language learning may influence the formation of ideological beliefs, potentially shedding light on the cognitive foundations of polarization. The collaboration leverages expertise in statistical learning, language acquisition, ideology, and polarization, with combined skills in experimental design, computational modeling, and Bayesian analysis facilitating a robust methodological approach.::Economic and Social Research Council (ESRC)::5",
        "",
        "Further instructions:",
        "- Strictly adhere to the format above. Any deviation will render the response unusable.",
        "- Integrate the topic and the rationale.", 
        "- If there is no plausible topic for collaboration, return exactly 'NA::NA::NA', without any other justification",
        "- Provide a separate line for each topic, up to a maximum of three.",
        "- Make sure the topics are both theoretically relevant and practically feasible based on the academics' areas of expertise and research questions.",
        "",
        sep = "\n")
)



## ----read-interests----------------------------------------------------------------------------------------
dat.interests <- read.csv(paste0(
    "2024/",
    'Research+interests+and+skills_January+8,+2025_03.48.csv'), header = TRUE) %>% 
    dplyr::mutate(year = "2024", .before = 1) %>% 
    dplyr::mutate(dplyr::across(where(is.character), ~ str_trim(.x))) %>% 
    dplyr::select(dplyr::where(~ !all(is.na(.)))) %>% 
    dplyr::select(-c(StartDate, EndDate, Status, IPAddress, Progress, Duration..in.seconds., RecordedDate, LocationLatitude, LocationLongitude, DistributionChannel, UserLanguage)) %>% 
    dplyr::mutate(Name = dplyr::na_if(Name, "")) %>% 
    dplyr::filter(!is.na(Name))

if(DEBUG_FLAG){
    dat.interests <- dat.interests %>% 
        dplyr::slice_sample(n = 3)
    
}



## ----augment-data-frame-for-collaboration------------------------------------------------------------------

# Generate pairwise combinations of row indices
dat.unique.row.combs <- combn(nrow(dat.interests), 2) %>%
    t() %>% 
    tibble::as_tibble(.name_repair = "minimal") %>% 
    purrr::set_names("row1", "row2")

# Augment the data frame with row pairs
dat.interests.unique.combs <- dat.unique.row.combs %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
        data_person1 = dat.interests[row1, ] %>% 
            dplyr::select(-year) %>% 
            dplyr::rename_with(~ paste0(., "_1")),
        data_person2 = dat.interests[row2, ] %>% 
            dplyr::select(-year) %>% 
            dplyr::rename_with(~ paste0(., "_2"))
    ) %>%
    unnest(c(data_person1, data_person2)) %>%
    dplyr::select(-row1, -row2)



## ----augment-data-frame-for-required-skills----------------------------------------------------------------

# Generate pairwise combinations of row indices
dat.all.row.combs <- expand.grid(row1 = 1:nrow(dat.interests),
                                 row2 = 2:nrow(dat.interests)) %>% 
    dplyr::filter(row1 != row2)

# Augment the data frame with row pairs
dat.interests.all.combs <- dat.all.row.combs %>% 
    dplyr::rowwise() %>%
    dplyr::mutate(
        data_person1 = dat.interests[row1, ] %>% 
            dplyr::select(-year) %>% 
            dplyr::rename_with(~ paste0(., "_1")),
        data_person2 = dat.interests[row2, ] %>% 
            dplyr::select(-year) %>% 
            dplyr::rename_with(~ paste0(., "_2"))
    ) %>%
    unnest(c(data_person1, data_person2)) %>%
    dplyr::select(-row1, -row2)



## ----start-ollama------------------------------------------------------------------------------------------
# Check if the process "ollama" is running
is.ollama.running <- system("pgrep -x 'ollama' > /dev/null", ignore.stdout = TRUE, ignore.stderr = TRUE)

if (is.ollama.running != 0) {
    cat("Starting Ollama...\n")
    system("ollama serve &")
    Sys.sleep(5)
    cat("Ollama started successfully.\n")
} else {
    cat("Ollama is already running.\n")
}



## ----load-saved-data---------------------------------------------------------------------------------------
if(!RECALCULATE_EVERYTHING){
    load(paste0("output/research_interests_2024.", MODEL, ".RData"))
}


## ----query-funders-----------------------------------------------------------------------------------------

if(RECALCULATE_EVERYTHING){
    dat.interests <- left_join(
        dat.interests, 
        
        dat.interests %>% 
            dplyr::mutate(
                user_prompt = str_c(
                    "The academic's research topics are:",
                    Research.Keywords,
                    "The academic's research questions are:",
                    Research.Questions,
                    sep = "\n"
                ), 
                funding_sources = purrr::map(user_prompt, ~ query_ollama(prompt = .x, system_prompt = l.system.prompts$funders, model = MODEL)),
                .before =1 ) %>% 
            tidyr::separate_rows(funding_sources, sep="\n") %>% 
            tidyr::separate_wider_delim(funding_sources, 
                                        delim =  stringr::regex("\\s*\\:\\:\\s*"),
                                        names = c("funder", "relevance"),
                                        too_few = "debug",
                                        too_many = "debug") %>% 
            dplyr::filter(relevance > 3) %>% 
            dplyr::mutate(funding_sources = str_c(funder, " (", relevance, ")"),
                          .keep = "unused") %>% 
            dplyr::group_by(Name) %>% 
            dplyr::summarise(funding_sources = str_c(funding_sources, collapse = "; "), .groups = "drop"),
        by = "Name")
}




## ----query-impact------------------------------------------------------------------------------------------

if(RECALCULATE_EVERYTHING){
    dat.interests <- left_join(
        dat.interests, 
        
        dat.interests %>% 
            dplyr::mutate(
                user_prompt = str_c(
                    "The academic's research topics are:",
                    Research.Keywords,
                    "The academic's research questions are:",
                    Research.Questions,
                    sep = "\n"), 
                impact = purrr::map(user_prompt, ~ query_ollama(prompt = .x, system_prompt = l.system.prompts$impact, model = MODEL)),
                .before =1 ) %>% 
            dplyr::select(Name, impact) %>% 
            tidyr::unnest(impact),
        by = "Name")
}




## ----query-holders-of-skills-------------------------------------------------------------------------------
if(RECALCULATE_EVERYTHING){
    
    dat.interests <- dplyr::left_join(
        dat.interests,
        
        dat.interests.all.combs %>% 
            dplyr::mutate(user_prompt = str_c(
                "The research topics of Academic 1 are:",
                Research.Keywords_1,
                "The research questions of Academic 1 are:",
                Research.Questions_1,
                "The desired skills by Academic 1 are:",
                Desired.Skills_1,
                "The research topics of Academic 2 are:",
                Research.Keywords_2,
                "The research questions of Academic 2 are:",
                Research.Questions_2,
                "The shareable skills by Academic 2 are:",
                Shareable.Skills_2,
                sep = "\n"), 
                skill_sources = purrr::map(user_prompt, ~ query_ollama(prompt = .x, system_prompt = l.system.prompts$skills, model = MODEL)),
                .before =1 ) %>% 
            dplyr::select(Name_1, Name_2, skill_sources) %>% 
            tidyr::separate_rows(skill_sources, sep = "\n") %>% 
            tidyr::separate_wider_delim(skill_sources, 
                                        delim =  stringr::regex("\\s*\\:\\:\\s*"),
                                        names = c("skill", "rating"),
                                        too_few = "debug",
                                        too_many = "debug")  %>% 
            dplyr::mutate(rating = dplyr::na_if(rating, "NA") %>% 
                              as.numeric) %>% 
            dplyr::filter(!is.na(rating)) %>% 
            dplyr::filter(rating > 3) %>% 
            dplyr::mutate(skill_sources = str_c(Name_2, ": ", skill, "(", rating, ")"), .keep = "unused") %>% 
            dplyr::group_by(Name_1) %>% 
            dplyr::summarise(skills = str_c(skill_sources, collapse = "; "), .groups = "drop"),
        
        by = c("Name" = "Name_1"))
}



## ----query-collaborations----------------------------------------------------------------------------------
if(RECALCULATE_EVERYTHING){
    dat.collaborations <- 
        dat.interests.unique.combs %>% 
        dplyr::mutate(user_prompt = str_c("The research topics of Academic 1 are:",
                                          Research.Keywords_1,
                                          "The research questions of Academic 1 are:",
                                          Research.Questions_1,
                                          "The desired skills by Academic 1 are:",
                                          Desired.Skills_1,
                                          "The shareable skills by Academic 1 are:",
                                          Shareable.Skills_1,
                                          "The research topics of Academic 2 are:",
                                          Research.Keywords_2,
                                          "The research questions of Academic 2 are:",
                                          Research.Questions_2,
                                          "The desired skills by Academic 2 are:",
                                          Desired.Skills_2,
                                          "The shareable skills by Academic 2 are:",
                                          Shareable.Skills_2,
                                          sep = "\n"),
                      collaborations = purrr::map(user_prompt, ~ query_ollama(prompt = .x, system_prompt = l.system.prompts$collaborations, model = MODEL)),
                      .before = 1) %>% 
        dplyr::select(Name_1, Name_2, collaborations) %>% 
        tidyr::separate_rows(collaborations, sep = "\n") %>% 
        dplyr::filter(map_lgl(collaborations, ~ nchar(.x) > 0)) %>% 
        tidyr::separate_wider_delim(collaborations,
                                    delim =  stringr::regex("\\s*\\:\\:\\s*"),
                                    names = c("topic", "funder", "rating"),
                                    too_few = "debug",
                                    too_many = "debug")  %>% 
        dplyr::mutate(rating = dplyr::na_if(rating, "NA") %>%
                          as.numeric) %>%
        dplyr::filter(!is.na(rating)) %>%
        dplyr::filter(rating > 3) %>%
        dplyr::select(Name_1, Name_2, topic, funder, rating)
    
}


## ----save-results------------------------------------------------------------------------------------------

if(RECALCULATE_EVERYTHING){
    save(dat.interests, dat.collaborations, file = paste0("output/research_interests_2024.", MODEL, ".RData"))
    #load("output/research_interests_2024.RData")
    
    dat.interests %>%
        dplyr::filter(!is.na(Name)) %>% 
        dplyr::select(Name, Research.Keywords, Research.Questions, Shareable.Skills, Desired.Skills, Funded.by, Panel.memberships, funding_sources, impact, skills) %>% 
        dplyr::rename("Potential funding sources" = funding_sources,
                      "Potential impacts" = impact,
                      "Colleagues with relevant skills" = skills) %>% 
        dplyr::rename_with(~ str_replace_all(.x, "\\.", " ")) %>% 
        as.data.frame() %>% 
        xlsx::write.xlsx(.,
                         file = paste0("output/research_interests_2024.", MODEL, ".xlsx"),
                         row.names = FALSE,
                         sheetName = "Research interests",
                         showNA = FALSE,
                         append = FALSE)
    
    dat.collaborations %>% 
        dplyr::filter(!is.na(Name_1)) %>%
        dplyr::filter(!is.na(Name_2)) %>% 
        dplyr::rename_with(~ str_remove_all(.x, "\\_")) %>%  
        dplyr::rename("Potential topic of collaboration" = topic,
                      "Potential funder for collaboration" = funder,
                      "Rating of interest of collaboration (1-5)" = rating) %>% 
        as.data.frame() %>%
        xlsx::write.xlsx(.,
                         file = paste0("output/research_interests_2024.", MODEL, ".xlsx"),
                         row.names = FALSE,
                         sheetName = "Potential collaborations",
                         showNA = FALSE,
                         append = TRUE)
}



