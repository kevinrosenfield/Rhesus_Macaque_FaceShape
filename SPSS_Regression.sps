* Encoding: UTF-8.
# After Loading the Facial Masculinity Data file
# Run regression with Sex as dependent and 8 facial features as predictors
# Save unstandardized predicted variables (masculiniy scores)

REGRESSION   
/MISSING LISTWISE   
/STATISTICS COEFF OUTS R ANOVA   
/CRITERIA=PIN(.05) POUT(.10)   
/NOORIGIN   
/DEPENDENT Sex_M_1_F_0   
/METHOD=ENTER Interpupil_distance Lower_face_height Eye_height Jaw_width Face_width   
  Temporalis_height Jaw_height Nose_length   
/SAVE PRED.