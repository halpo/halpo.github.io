shield.io.src <-
function( repo
        , category
        , ...
        , domain = "github"
        , user = gh_whoami()$login
        , style = NULL
        , color = NULL
        , label = NULL
        , logo = NULL
        , logo.color = NULL
        ){
    path <- glue_collapse(c(domain, category, user, repo, ...), "/")
    opts <- glue_collapse( c( if(!is.null(color))      glue("color={color}")
                            , if(!is.null(style))      glue("style={style}")
                            , if(!is.null(label))      glue("label={label}")
                            , if(!is.null(logo))       glue("logo={logo}")
                            , if(!is.null(logo.color)) glue("logoColor={logo.color}")
                            ), sep="&")
    if (length(opts))
        glue("https://img.shields.io/{path}.svg?{opts}")
    else
        glue("https://img.shields.io/{path}.svg")
}
if(FALSE){#@testing
    x <- shield.io.src("cursory", "issues", "halpo")
    expect_equal(x, "https://img.shields.io/github/issues/halpo/cursory.svg")

    x <- shield.io.src("cursory", "issues", "halpo", color='red')
    expect_equal(x, "https://img.shields.io/github/issues/halpo/cursory.svg?color=red")

    x <- shield.io.src("cursory", "issues", "halpo", color='red', logo='travis')
    expect_equal(x, "https://img.shields.io/github/issues/halpo/cursory.svg?color=red&logo=travis")
}

shield <- function(alt, ..., href=NULL){
    src <- shield.io.src(...)
    if(is.null(href))
        glue::glue("![{alt}]({src})")
    else
        glue::glue("[![{alt}]({src})]({href})")
}


activity_shield <-
function( repo
        , type = c( "last-commit"
                  , "commits-since"
                  , "commit-activity"
                  )
        , user = gh_whoami()$login
        , href = glue("https://github.com/{user}/{repo}")
        ){
    assert_that(is.string(repo))
    type <- match.arg(type)
    make_md_badge( type
                 , href
                 , paste0(paste(c("https://img.shields.io/github"
                                 , type, user, repo, version, branch)
                               , collapse='/')
                         , ".svg")
                 )
}
if(FALSE){#@testing
    x <- make_activity_shield('cursory', 'last-commit', user='halpo')
    testthat::expect_equal(x, "[![last-commit](https://github.com/halpo/cursory)](https://img.shields.io/github/last-commit/halpo/cursory.svg)")
}

travis_shield <- function(repo,...){
    shield("Travis build status", repo=repo, category=NULL, domain='travis', ..., logo='travis')
}
if(FALSE){#@testing
    val <- travis_shield('cursory')
    expect_equal(val, "![Travis build status](https://img.shields.io/travis/halpo/cursory.svg?logo=travis)")
}

codecov_shield <-
function( repo,...
        , user = gh_whoami()$login
        , vcs = 'gh'
        , branch = NULL
        ){
    if (is.null(branch)) {
        url <- glue("https://codecov.io/{vcs}/{user}/{repo}")
        shield("Code Coverage", repo, vcs, ..., user=user, domain='codecov/c', href=url)
    } else {
        url <- glue("https://codecov.io/gh/{user}/{repo}?branch={branch}")
        shield("Code Coverage", repo, ..., branch, domain="", user=user, href=url)
    }
}
if(FALSE){#@testing
    val <- codecov_shield('cursory')
    expect_equal(val, "[![Code Coverage](https://img.shields.io/codecov/c/gh/halpo/cursory.svg)](https://codecov.io/gh/halpo/cursory)")

    val <- codecov_shield('cursory', vcs='github')
    expect_equal(val, "[![Code Coverage](https://img.shields.io/codecov/c/github/halpo/cursory.svg)](https://codecov.io/github/halpo/cursory)")
}


issues_shield <- function( repo,..., user = gh_whoami()$login){
    url <- glue("https://github.com/{user}/{repo}/issues")
    shield("Open Issues", repo, "issues", ..., user=user, href=url)
}
if(FALSE){#@testing
    val <- issues_shield("cursory", user="halpo")
    expect_equal(val, "[![Open Issues](https://img.shields.io/github/issues/halpo/cursory.svg)](https://github.com/halpo/cursory/issues)")
}

pull_requests_shield <- function( repo,..., user = gh_whoami()$login){
    url <- glue("https://github.com/{user}/{repo}/pulls")
    shield("Open Issues", repo, "issues-pr", ..., user=user, href=url)
}
if(FALSE){#@testing
    val <- pull_requests_shield("cursory", user="halpo")
    expect_equal(val, "[![Open Issues](https://img.shields.io/github/issues-pr/halpo/cursory.svg)](https://github.com/halpo/cursory/pulls)")
}

stars_shield <- function(repo, ..., style="social"){
    shield("Stars", repo, "stars", style=style)
}
watchers_shield <- function(repo, ..., style="social", label='Watching'){
    shield("watchers", repo, "watchers", style=style, label=label)
}

cran_shield <- function(repo, ..., user=NULL, logo="R")
    shield("CRAN", repo, category="v", domain="cran", user=NULL, logo=logo, logo.color='blue', label="CRAN"
          , href = glue("https://cran.r-project.org/package={repo}"))
