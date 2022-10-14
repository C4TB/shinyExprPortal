# Development Guidelines and Roadmap for clinvisx

Created: 01/03/2021  
Latest update: 14/10/2022

## Design principles

This section describes the principles guiding the design of the modules of clinvsix. The aim is to establish the development boundaries (i.e. how much computation should be done within the app) and general readability vs. performance decisions.

### 1 Prioritise base R

The app, as it is, already imports a relatively high (but not massive) number of packages, and the aim is to avoid including new dependencies as much as possible for maintenance and customisation purposes. It is better to preserve a cohesive app, modifying internal functions as required to support new functionality, rather than have modules with similar computations that rely on different packages.

The second reason is performance. It is tempting to use packages that include wrappers and convenience functions, e.g. to compute correlation and p-values simultaneously. With large expression matrices, it is probably better to use base R in most cases.

### 2 Reactives vs functions

The priority when writing a module is to separate reactive selections from the computations. The code inside reactive blocks will only serve to evaluate the required inputs and pass the arguments to functions. Example:

    computedData <- eventReactive(input$compute_button, { 
      
      req(input$slider_value)
      
      myFilterFunction(expression_matrix, input$slider_value)
    
    })
  
    output$plot <- renderPlot({ 
    
     plotBarChart(computedData())
    
    })
    
### 3 On functions

**Split and combine functions as needed:** If a plot or function result is similar to another, it is good to combine them in a single function with varying parameters. At the same time, it's also a good idea to reduce a function to a bare minimum across modules, and have additional functions to complement it. Example: a basic scatterplot with a bit of customisation as the minimum function, with faceting and geom_smooth in separate functions.  

**Placement:** It is also better to leave module-specific functions in a module-specific file. In the future they can be moved to generic files if used across modules.

**Naming:** Most of the internal functions will not be exported, so it's preferable to give longer but descriptive names such as selectGenesIfTrue, instead of a generic name such as "filter".

### 4 Analysis results vs exploration trade-offs

The app enables visual exploration of data as well as results of the analysis, but it is not an actual analysis tool to do, e.g. clustering or differential expression analysis. Any results that require additional, non-standard packages (e.g. limma) should be computed separately and results included in files. Potential exceptions are those when file sizes are significantly higher than the computation time.

## Development Roadmap

The first version of clinvisx is addressing the needs of PSORT and RAMAP. The longer term vision is to make it configurable for any study along the lines of these (transcriptome & clinical analysis)

#### Version 2.0 goals

- [ ] **API support to reduce data loading:** the current version of clinvisx requires loading all the data into the app, and the files must be in a location accessible by Shiny Server. There are two potential API-based models: retrieve data from a remote location and do selections and computations locally (in the Shiny server), or delegate almost every server-side computation to the API server.

- [ ] **Database support:** an alternative to API support is to support loading data from a database, with selections and summaries done in the database.