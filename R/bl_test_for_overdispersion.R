#' bl_test_for_overdispersion
#'
#'@description Testing for overdispersion/computing overdispersion factor with the usual caveats, plus a few extras – counting degrees of freedom, etc. – the usual procedure of calculating the sum of squared Pearson residuals and comparing it to the residual degrees of freedom should give at least a crude idea of overdispersion. The following attempt counts each variance or covariance parameter as one model degree of freedom and presents the sum of squared Pearson residuals, the ratio of (SSQ residuals/rdf), the residual df, and the p-value based on the (approximately!!) appropriate χ2 distribution. Do PLEASE note the usual, and extra, caveats noted here: this is an APPROXIMATE estimate of an overdispersion parameter. Even in the GLM case, the expected deviance per point equaling 1 is only true as the distribution of individual deviates approaches normality, i.e. the usual λ>5 rules of thumb for Poisson values and min(Np,N(1−p))>5 for binomial values (e.g. see Venables and Ripley (2002), p. 208-209). (And that’s without the extra complexities due to GLMM, i.e. the “effective” residual df should be large enough to make the sums of squares converge on a χ2
#' distribution …)
#' Remember that (1) overdispersion is irrelevant for models that estimate a scale parameter (i.e. almost anything but Poisson or binomial: Gaussian, Gamma, negative binomial …) and (2) overdispersion is not estimable (and hence practically irrelevant) for Bernoulli models (= binary data = binomial with N=1).
#'     The recipes below may need adjustment for some of the more complex model types allowed by glmmTMB (e.g. zero-inflation/variable dispersion), where it’s less clear what to measure to estimate overdispersion.
#'
#' The following function should work for a variety of model types (at least glmmADMB, glmmTMB, lme4, …).
#' see [Ben Bolkers Github](https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#'model-definition).
#'
#'
#' @param model A regression model
#' @export
#' @examples
#' # a model
#' multivar_model <- lme4::glmer(grasshopper ~ elevation + heat_load + bare_ground + (1|sub_area), data = bl_ecology_data, family = "poisson")
#' bl_test_for_overdispersion (multivar_model)
#'
#' bl_test_for_overdispersion (multivar_model)

bl_test_for_overdispersion <- function(model) {
    rdf <- df.residual(model)
    rp <- residuals(model,type="pearson")
    Pearson.chisq <- sum(rp^2) %>% bl_round_beautifully () %>% as.numeric ()
    prat <- Pearson.chisq/rdf %>% bl_round_beautifully () %>% as.numeric ()
    pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE) %>% bl_round_beautifully () %>% as.numeric ()

    return (data.frame(chisq=Pearson.chisq %>% bl_round_beautifully () %>% as.numeric (),
                 residual_df=rdf %>% bl_round_beautifully () %>% as.numeric (),
                 ratio=prat %>% bl_round_beautifully () %>% as.numeric (),
                 p=pval %>% bl_round_beautifully () %>% as.numeric ()))
}

