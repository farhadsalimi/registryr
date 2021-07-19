#' Perform a stepwise logistic or linear regression using LRT tests of significance.
#' @description Copied and edited from: https://msu.edu/~rubin/code/stepwise_demo.nb.html
#'
#' @param out_var the outcome variable
#' @param ind_var the independent variables to consider
#' @param pe the significance level above which a variable may enter the model
#' @param pr the significance level below which a variable may be deleted from the model
#' @param data the data frame to use
#' @param model the model that needs to be performed ("logistic" or "linear")
#'
#' @return the final model
#' @export
#'
#' @examples

stepwise <-
  function(out_var, ind_var, pe = 0.01, pr = 0.05, data = NULL, model = "logistic") {
    # Sanity check: alpha.to.enter should not be greater than alpha.to.leave
    if (pe > pr) {
      warning("Your alpha-to-enter is greater than your alpha-to-leave,
              which could throw the function into an infinite loop.\n")
      return(NA)
    }

    if (model != "logistic" & model != "linear") {
      stop("You model could not be found,
              logistic or linear are the only options\n")
    }

    if (model == "logistic") {
      # Fit the full model

      full_model <-
        stats::glm(as.formula(paste(out_var, paste(ind_var, collapse = " + "), sep = " ~ ")),
          family = binomial(link = "logit"),
          data = data
        )

      # Fit the initial model
      current_model <-
        stats::glm(as.formula(paste(out_var, 1, sep = " ~ ")),
          family = binomial(link = "logit"),
          data = data
        )
    } else {
      # Fit the full model
      full_model <-
        stats::glm(as.formula(paste(out_var, paste(ind_var, collapse = " + "), sep = " ~ ")),
          family = gaussian(link = "identity"),
          data = data
        )

      # Fit the initial model
      current_model <-
        stats::glm(as.formula(paste(out_var, 1, sep = " ~ ")),
          family = gaussian(link = "identity"),
          data = data
        )
    }

    # Process consecutive models until we break out of the loop
    while (TRUE) {

      # Print the model description.
      print(summary(current_model)$coefficients)

      # Get the size of the current model
      n_terms <- dim(summary(current_model)$coefficients)[1]
      # Try to drop a term (but only if more than one is left)
      if (n_terms > 1) {
        # Look for terms that can be dropped based on F tests
        d <- drop1(current_model, test = "LRT")
        # Find the term with largest p-value
        pmax <- suppressWarnings(max(d[, "Pr(>Chi)"], na.rm = TRUE))

        # Try to drop a term (but only if more than one is left)
        if (pmax > pr) {
          # We have a candidate for deletion
          # Get the name of the variable to delete
          var <- rownames(d)[d[, "Pr(>Chi)"] == pmax]
          # If an intercept is present, it will be the first name in the list
          # There also could be ties for worst p-value
          # Taking the second entry if there is more than one is a safe solution to both issues
          if (length(var) > 1) {
            var <- var[2]
          }
          # Print out the variable to be dropped
          write(paste("--- Dropping", var, "\n"), file = "")
          # Modify the formula to drop the chosen variable (by subtracting it from the current formula)
          f <- formula(current_model)
          f <- as.formula(paste(f[2], "~", paste(f[3], var, sep = " - ")), env = environment(f))
          # Fit the modified model and loop
          # Fit the modified model and loop
          if (model == "logistic") {
            current_model <-
              glm(
                formula = f,
                family = binomial(link = "logit"),
                data = data
              )
          } else {
            current_model <-
              glm(
                formula = f,
                family = gaussian(link = "identity"),
                data = data
              )
          }

          next
        }
      }

      # If we get here, we failed to drop a term; try adding one
      # Note: add1 throws an error if nothing can be added (current == full), which we trap with tryCatch
      a <- tryCatch(
        add1(current_model, full_model, test = "LRT"),
        error = function(e) NULL
      )

      if (is.null(a)) {
        # There are no unused variables (or something went splat), so we bail out.
        break
      }
      # Find the minimum p-value of any term (skipping the terms with no p-value).
      # In case none of the remaining terms have a p-value (true of the intercept and any
      # linearly dependent predictors), suppress warnings about an empty list. The test for a
      # suitable candidate to drop will fail since pmin will be set to infinity.
      pmin <- suppressWarnings(min(a[, "Pr(>Chi)"], na.rm = TRUE))
      if (pmin < pe) {
        # We have a candidate for addition to the model. Get the variable's name
        var <- rownames(a)[a[, "Pr(>Chi)"] == pmin]
        # We have the same issue with ties and the presence of an intercept term,
        # and the same solution, as above
        if (length(var) > 1) {
          var <- var[2]
        }
        # Print the variable being added
        write(paste("+++ Adding", var, "\n"), file = "")

        # Add it to the current formula.
        f <- formula(current_model)
        f <- as.formula(paste(f[2], "~", paste(f[3], var, sep = " + ")), env = environment(f))

        # Fit the modified model and loop.
        if (model == "logistic") {
          current_model <-
            glm(
              formula = f,
              family = binomial(link = "logit"),
              data = data
            )
        } else {
          current_model <-
            glm(
              formula = f,
              family = gaussian(link = "identity"),
              data = data
            )
        }

        next
      }
      # If we get here, we failed to make any changes to the model; time to declare victory and exit.
      break
    }
    current_model
  }
