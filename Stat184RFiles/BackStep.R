#runs code for BIC backwards elim. WARNING: computationally expensive, takes time
parsimoniousmodel <- step(naivemodel, k=log(20))

# Extract null and residual deviance
null_deviance <- parsimoniousmodel$null.deviance
residual_deviance <- parsimoniousmodel$deviance

# Compute degrees of freedom
df_null <- parsimoniousmodel$df.null
df_residual <- parsimoniousmodel$df.residual
df_diff <- df_null - df_residual