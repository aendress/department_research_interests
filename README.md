# department_research_interests

This scripts generates ideas for funders, impact and collaborations based on research interests.

The attached R/Rmarkdown scripts use llama3.2 and mistral to suggest (1) potential funders, (2) potential impact (as defined by the UK REF), (3) colleagues with potentially relevant skills, and (4) potential collaborators within a psychology department. 

The script just calls ollama [https://ollama.com/] to run the large language models. See script for the prompts. 

The `R` file has been generated from the `Rmd` file through `knitr::purl`, as it is much faster to run it on the command line than in the GUI. 

### Usage:
```bash
Rscript department_research_interests.R <model> <top_p>`
```

### Parameters (Obligatory)
* `<model>`: (Required) The name of the model to use (e.g., "mistral").
* `<top_p>`: (Required) A numeric value for the top-p sampling parameter (e.g., 0.7).

### Example:
```bash
Rscript department_research_interests.R mistral .7
```