# 99_subjects_to_remove.R
# selected based on multiple high correlations and picking relatively rarer (see 99_correlations.R)
# called by 3_data_prepare.R
# Nov 2025

to_remove = c('Lentivirus','Immunodeficiency viruses','Energy-producing organelles','Natural antisense transcripts',
              'SARS coronavirus','SARS CoV 2', # both covered by 'Coronaviruses'
              'Anxiety disorders', # same as Neuroses
              'Crime', # almost same as Criminology
              'Escherichia coli', # almost same as Escherichia
              'Polymers', # almost same as Macromolecules
              'Drosophila melanogaster', # almost same as Drosophila 
              'Stroke', # almost same as Cerebrovascular diseases 
              'Sequence analysis', # almost same as Bioinformatics
              'Careers in research', # almost same as Science and technology workforce
              'Sediment', # almost same as Petrology
              'Microbial pathogens', # almost same as Pathogens
              'Fluid dynamics', # almost same as Continuum mechanics   
              'Crystallography', # same as Solid state physics
              'Organic chemistry', # same as Organic compounds'
              'Diabetic endocrinology', # same as Insulin
              'Breast cancer', # same as Breast cancer
              'Apicomplexa', # same as Parasite groups
              'Prostate diseases', # same as 'Prostate cancer'
              'Exercise', # almost same as Physical fitness
              'Toxins', # almost same as toxic agents
              'Viral pathogens', # very close of Viruses
              'Landforms', # very similar to topography
              'Mood disorders', # similar to depression
              'Retroviruses', # big overlap with HIV
              'Endocrine disorders', # very similar to Diabetes mellitus
              'Transcription factors', # almost same as Regulatory proteins
              'Parasitic protozoans', # almost the same as 'protozoans'
              'Experimental organism systems', # almost same as Animal studies
              'Continuum mechanics', # almost same as 'Fluid mechanics'
              'Cytokines', # overlap with Innate immune system and Molecular development
              'Artificial gene amplification and extension', # strong overlap with PCR
              'Flow cytometry', # overlap with Cytophotometry
              'Diagnostic radiology', # very similar to  Radiology and imaging
              'Evolutionary systematics', # very similar to Phylogenetics 
              'Cellular neuroscience' # very similar to Neurons
)