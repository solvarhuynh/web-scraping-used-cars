# Project File Overview

Below is a concise, human‑readable description of every important file in the **project** directory (excluding internal Git metadata). Each entry lists the file’s location, its role, and the main capabilities it provides to the data‑pipeline system.

| File (relative to project root)        | Description/Purpose                                                                                                                     
|----------------------------------------|----------------------------------------------------------------------------|
| **README.md**                          | High‑level project description, usage notes, and quick‑start instructions for developers. 

| **run_pipeline.R**                     | Orchestrates the full batch pipeline (optional scraping → cleaning → validation → DB init → merge).
                      
| **run_realtime.R**                     | Entry point for the real‑time update jobs (delta‑fetching).
                                          
| **script/utils.R**                     | Shared utility library (logging, directory helpers, schema definitions, and cleaning functions).
     
| **script/init_database.R**             | Initializes per-source SQLite databases in `web_scraping/data/init_db/` from clean CSV files.
            
| **script/merge_data.R**                | Merges per-source SQLite databases into `web_scraping/data/master_data.db` and `web_scraping/data/master_data.csv`, handling deduplication on `url`. 

| **script/clean/clean_chotot.R**        | Cleans raw Chợ Tốt data into a standardized format.
                                                  
| **script/clean/clean_carpla.R**        | Cleans raw Carpla data into a standardized format.
                                                   
| **script/clean/clean_banxehoicu.R**    | Cleans raw Bán Xe Hơi Cũ data into a standardized format.
                                            
| **script/scrap/scrap_chotot.R**        | Batch scraper for Chợ Tốt: uses **Chromote** to extract listing details and writes to `data/raw/`.
   
| **script/scrap/scrap_carpla.R**        | Batch scraper for Carpla: uses **Chromote** to extract listing details and writes to `data/raw/`.
   
| **script/scrap/scrap_banxehoicu.R**    | Static‑HTML scraper for Bán Xe Hơi Cũ: uses **httr/rvest** with checkpoint handling.
                 
| **script/realtime/realtime_chotot.R**  | Real‑time delta‑fetcher for Chợ Tốt: checks Page 1 against SQLite and inserts new rows until a duplicate is found.
                                               |
| **script/realtime/realtime_carpla.R**  | Real‑time delta‑fetcher for Carpla (same logic as Chợ Tốt).
                                          
| **script/realtime/realtime_banxehoicu.R**| Real‑time delta‑fetcher for Bán Xe Hơi Cũ (static HTML).
                                           
| **data/clean/data_chotot_clean.csv**   | Cleaned Chợ Tốt listings ready for merging.
                                                          
| **data/clean/data_carpla_clean.csv**   | Cleaned Carpla listings.
                                                                             
| **data/clean/data_banxehoicu_clean.csv**| Cleaned Bán Xe Hơi Cũ listings.
                                                                     
| **data/clean/data_bonbanh_clean.csv**  | Sample cleaned dataset (kept for reference).
                                                         
| **data/master_data.csv**               | Result of the merge step – consolidated master file used by visualization, modeling, and the app.
           
| **data/raw/data_chotot_raw.csv**       | Raw CSV output from the Chợ Tốt batch scraper.
                                                       
| **data/raw/data_carpla_raw.csv**       | Raw CSV output from the Carpla batch scraper.
                                                        
| **data/raw/data_banxehoicu_raw.csv**   | Raw CSV output from the Bán Xe Hơi Cũ batch scraper.
                                                 
| **rule/clean_rule.md**                 | Specification of data cleaning and normalization rules.
                                              
| **rule/scrap_rule.md**                 | Guidelines for scraping (pagination limits, intervals, and checkpoint handling).
                     
| **rule/process_rule.md**               | High‑level data processing workflow description (Scrape → Clean → Merge → DB).
                       
| **rule/realtime_rule.md**              | Detailed policy for real‑time ingestion: SQLite storage, delta‑fetching, and session management.
     
| **rule/files_explain.md**              | *This file* – provides an overview of the project file structure.
                                                            
| **.git/**                             | Standard Git repository metadata.
                                                                    

---

### How to Use This Overview

*   **Developers**: Use this table to locate the script responsible for a specific stage (Scrape, Clean, Merge, Realtime, DB).
*   **Newcomers**: Start with `README.md` for setup, then run `run_pipeline.R` for a full batch update or `run_realtime.R` for incremental updates.
*   **Adding New Sources**: Create a batch scraper in `script/scrap/`, a matching cleaner in `script/clean/`, and a realtime script in `script/realtime/`. Real‑time scripts should reuse helper functions from batch scrapers.
