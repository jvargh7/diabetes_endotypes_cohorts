dbGetQuery(con_Cosmos,

            "SELECT TOP 1000 *
           FROM PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].decdat03 dd
           INNER JOIN dbo.DiagnosisEventFact def
           ON def.PatientDurableKey = dd.PatientDurableKey
           INNER JOIN dbo.DiagnosisTerminologyDim dtd
	         ON def.DiagnosisKey = dtd.DiagnosisKey")

