

# Utilities -------------------------------------------------------

make_md_badge <- function(name, href, src){
    as.character(glue::glue("[![{name}]({src})]({href})"))
}

make_badges <-
function( pkg
        , user = gh_whoami()$login
        , repo = pkg
        , branch='master'
        , stage = NULL
        ){
    tibble( Package  = pkg
          , Latest   = unclass(make_activity_shield(repo, 'last-commit', user, branch=branch))
          , Travis   = unclass(make_badge_travis(repo, user=user))
          , Coverage = unclass(make_badge_codecov(repo, user=user, branch=branch))
          , CRAN     = unclass(make_badge_cran(pkg))
          )
}


# Specific Badges -------------------------------------------------

make_badge_cran <- function(pkg){
    make_md_badge( glue("CRAN Status")
                 , glue("https://CRAN.R-project.org/package={pkg}")
                 , glue("https://www.r-pkg.org/badges/version/{pkg}")
                 )
}
make_badge_lifecycle <- local({
    stages <- usethis:::stages
    function( pkg
            , stage=names(stages)
            , colour = stages[[stage]]
            ){
        make_md_badge( glue("Lifecycle Stage: {stage}")
                     , glue("https://img.shields.io/badge/lifecycle-{stage}-{colour}.svg")
                     , glue("https://www.tidyverse.org/lifecycle/#{stage}")
                     )
}})
make_badge_travis <- function(repo, user=gh_whoami()$login, ext=c('org', 'com')){
    ext <- match.arg(ext)
    url <- glue("https://travis-ci.{ext}/{user}/{repo}")
    img <- glue("{url}.svg?branch=master")
    make_md_badge("Travis build status", url, img)
}
make_badge_codecov <- function(repo, user=gh_whoami()$login, branch='master'){
    url <- glue("https://codecov.io/gh/{user}/{repo}?branch={branch}")
    img <- glue("https://codecov.io/gh/{user}/{repo}/branch/{branch}/graph/badge.svg")
    make_md_badge("Codecov test coverage", url, img)
}


