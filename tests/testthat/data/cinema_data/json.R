schema <- "../../cinema/cinema_schema.json"

json_valid_binary <- '
{
  "project": {
    "CM": {
      "contributionMatrices": {
        "hatmatrix": {
          "colNames": [
            "sleep:salad",
            "sleep:exercise",
            "salad:exercise"
          ],
          "colNamesNMAresults": [
            "Direct",
            "DirectL",
            "DirectU",
            "Indirect",
            "IndirectL",
            "IndirectU",
            "SideIF",
            "SideIFlower",
            "SideIFupper",
            "SideZ",
            "SidePvalue",
            "PropDir",
            "NMA treatment effect",
            "se treat effect",
            "lower CI",
            "upper CI",
            "lower PrI",
            "upper PrI",
            "PropDirNetmeta",
            "_row"
          ],
          "H": [
            [ 0.1, 0.2, 0.3 ],
            [ 0.11, 0.22, 0.33 ],
            [ 0.111, 0.222, 0.333 ]
          ],
          "model": "fixed",
          "NMAresults": [
            {
              "Direct": -0.1,
              "DirectL": -0.1,
              "DirectU": -0.1,
              "Indirect": -1.1,
              "IndirectL": -1.1,
              "IndirectU": -1.1,
              "SideIF": 0.1,
              "SideIFlower": -0.1,
              "SideIFupper": 0.1,
              "SideZ": 0.1,
              "SidePvalue": 0.1,
              "PropDir": 0.1,
              "NMA treatment effect": -0.1,
              "se treat effect": 0.1,
              "lower CI": -0.1,
              "upper CI": -0.1,
              "lower PrI": -0.1,
              "upper PrI": -0.1,
              "PropDirNetmeta": 0.1,
              "_row": "sleep:salad"
            },
            {
              "Indirect": -2.2,
              "IndirectL": -2.2,
              "IndirectU": -2.2,
              "PropDir": 0.2,
              "NMA treatment effect": -0.2,
              "se treat effect": 0.2,
              "lower CI": -0.2,
              "upper CI": -0.2,
              "lower PrI": -0.2,
              "upper PrI": -0.2,
              "PropDirNetmeta": 0.2,
              "_row": "sleep:exercise"
            },
            {
              "Direct": -0.3,
              "DirectL": -0.3,
              "DirectU": -0.3,
              "PropDir": 0.3,
              "NMA treatment effect": -0.3,
              "se treat effect": 0.3,
              "lower CI": -0.3,
              "upper CI": -0.3,
              "lower PrI": -0.3,
              "upper PrI": -0.3,
              "PropDirNetmeta": 0.3,
              "_row": "salad:exercise"
            }
          ],
          "rowNames": [
            "sleep:salad",
            "sleep:exercise",
            "salad:exercise"
          ],
          "rowNamesNMAresults": [
            "sleep:salad",
            "sleep:exercise",
            "salad:exercise"
          ],
          "sm": "OR"
        },
        "studycontributions": {
          "sleep:salad": {
            "Sleepy celery": 0.8,
            "Sleepy saunter": 0.1,
            "celery saunter": 0.1
          },
          "sleep:exercise": {
            "Sleepy celery": 0.1,
            "Sleepy saunter": 0.8,
            "celery saunter": 0.1
          },
          "salad:exercise": {
            "Sleepy celery": 0.1,
            "Sleepy saunter": 0.1,
            "celery saunter": 0.8
          }
        }
      }
    },
    "format": "long",
    "type": "binary",
    "studies": {
      "long": [
        {
          "study": "Sleepy celery",
          "id": 1,
          "t": "sleep",
          "r": 4,
          "n": 10,
          "rob": 1,
          "indirectness": 3
        },
        {
          "study": "Sleepy celery",
          "id": 1,
          "t": "salad",
          "r": 2,
          "n": 10,
          "rob": 1,
          "indirectness": 3
        },
        {
          "study": "Sleepy saunter",
          "id": 2,
          "t": "sleep",
          "r": 4,
          "n": 10,
          "rob": 2,
          "indirectness": 2
        },
        {
          "study": "Sleepy saunter",
          "id": 2,
          "t": "exercise",
          "r": 8,
          "n": 10,
          "rob": 2,
          "indirectness": 2
        },
        
        {
          "study": "celery saunter",
          "id": 3,
          "t": "salad",
          "r": 2,
          "n": 10,
          "rob": 3,
          "indirectness": 1
        },
        {
          "study": "celery saunter",
          "id": 3,
          "t": "exercise",
          "r": 8,
          "n": 10,
          "rob": 3,
          "indirectness": 1
        }
      ]
    }
  }
}
'

json_valid_continuous <- '
{
  "project": {
    "CM": {
      "contributionMatrices": {
        "hatmatrix": {
          "colNames": [
            "sleep:salad",
            "sleep:exercise",
            "salad:exercise"
          ],
          "colNamesNMAresults": [
            "Direct",
            "DirectL",
            "DirectU",
            "Indirect",
            "IndirectL",
            "IndirectU",
            "SideIF",
            "SideIFlower",
            "SideIFupper",
            "SideZ",
            "SidePvalue",
            "PropDir",
            "NMA treatment effect",
            "se treat effect",
            "lower CI",
            "upper CI",
            "lower PrI",
            "upper PrI",
            "PropDirNetmeta",
            "_row"
          ],
          "H": [
            [ 0.1, 0.2, 0.3 ],
            [ 0.11, 0.22, 0.33 ],
            [ 0.111, 0.222, 0.333 ]
          ],
          "model": "random",
          "NMAresults": [
            {
              "Direct": -0.1,
              "DirectL": -0.1,
              "DirectU": -0.1,
              "Indirect": -1.1,
              "IndirectL": -1.1,
              "IndirectU": -1.1,
              "SideIF": 0.1,
              "SideIFlower": -0.1,
              "SideIFupper": 0.1,
              "SideZ": 0.1,
              "SidePvalue": 0.1,
              "PropDir": 0.1,
              "NMA treatment effect": -0.1,
              "se treat effect": 0.1,
              "lower CI": -0.1,
              "upper CI": -0.1,
              "lower PrI": -0.1,
              "upper PrI": -0.1,
              "PropDirNetmeta": 0.1,
              "_row": "sleep:salad"
            },
            {
              "Indirect": -2.2,
              "IndirectL": -2.2,
              "IndirectU": -2.2,
              "PropDir": 0.2,
              "NMA treatment effect": -0.2,
              "se treat effect": 0.2,
              "lower CI": -0.2,
              "upper CI": -0.2,
              "lower PrI": -0.2,
              "upper PrI": -0.2,
              "PropDirNetmeta": 0.2,
              "_row": "sleep:exercise"
            },
            {
              "Direct": -0.3,
              "DirectL": -0.3,
              "DirectU": -0.3,
              "PropDir": 0.3,
              "NMA treatment effect": -0.3,
              "se treat effect": 0.3,
              "lower CI": -0.3,
              "upper CI": -0.3,
              "lower PrI": -0.3,
              "upper PrI": -0.3,
              "PropDirNetmeta": 0.3,
              "_row": "salad:exercise"
            }
          ],
          "rowNames": [
            "sleep:salad",
            "sleep:exercise",
            "salad:exercise"
          ],
          "rowNamesNMAresults": [
            "sleep:salad",
            "sleep:exercise",
            "salad:exercise"
          ],
          "sm": "MD"
        },
        "studycontributions": {
          "sleep:salad": {
            "Sleepy celery": 0.8,
            "Sleepy saunter": 0.1,
            "celery saunter": 0.1
          },
          "sleep:exercise": {
            "Sleepy celery": 0.1,
            "Sleepy saunter": 0.8,
            "celery saunter": 0.1
          },
          "salad:exercise": {
            "Sleepy celery": 0.1,
            "Sleepy saunter": 0.1,
            "celery saunter": 0.8
          }
        }
      }
    },
    "format": "long",
    "type": "continuous",
    "studies": {
      "long": [
        {
          "study": "Sleepy celery",
          "id": 1,
          "t": "sleep",
          "n": 10,
          "mean": 4.9,
          "sd": 1.2,
          "rob": 1,
          "indirectness": 3
        },
        {
          "study": "Sleepy celery",
          "id": 1,
          "t": "salad",
          "n": 10,
          "mean": 4.9,
          "sd": 1.2,
          "rob": 1,
          "indirectness": 3
        },
        {
          "study": "Sleepy saunter",
          "id": 2,
          "t": "sleep",
          "n": 10,
          "mean": 4.9,
          "sd": 1.2,
          "rob": 2,
          "indirectness": 2
        },
        {
          "study": "Sleepy saunter",
          "id": 2,
          "t": "exercise",
          "n": 10,
          "mean": 4.9,
          "sd": 1.2,
          "rob": 2,
          "indirectness": 2
        },
        
        {
          "study": "celery saunter",
          "id": 3,
          "t": "salad",
          "n": 10,
          "mean": 4.9,
          "sd": 1.2,
          "rob": 3,
          "indirectness": 1
        },
        {
          "study": "celery saunter",
          "id": 3,
          "t": "exercise",
          "n": 10,
          "mean": 4.9,
          "sd": 1.2,
          "rob": 3,
          "indirectness": 1
        }
      ]
    }
  }
}
'

json_incorrect_types <- '
{
  "project": {
    "CM": {
      "contributionMatrices": {
        "hatmatrix": {
          "colNames": 22.3,
          "colNamesNMAresults": 4,
          "H": [
            [ 0.1, 0.2, 0.3 ],
            [ "eleven", 0.22, 0.33 ],
            [ 0.111, 0.222, 0.333 ]
          ],
          "model": [ 1, 2, 3 ],
          "NMAresults": [
            {
              "Direct": true,
              "DirectL": -0.1,
              "DirectU": -0.1,
              "Indirect": -1.1,
              "IndirectL": -1.1,
              "IndirectU": -1.1,
              "SideIF": 0.1,
              "SideIFlower": -0.1,
              "SideIFupper": 0.1,
              "SideZ": 0.1,
              "SidePvalue": 0.1,
              "PropDir": 0.1,
              "NMA treatment effect": -0.1,
              "se treat effect": 0.1,
              "lower CI": -0.1,
              "upper CI": -0.1,
              "lower PrI": -0.1,
              "upper PrI": -0.1,
              "PropDirNetmeta": 0.1,
              "_row": 444
            }
          ],
          "rowNames": [
            "sleep:salad",
            "sleep:exercise",
            false
          ],
          "rowNamesNMAresults": [
            "sleep:salad",
            "sleep:exercise",
            444
          ],
          "sm": true
        },
        "studycontributions": {
          "sleep:salad": {
            "Sleepy celery": 0.8,
            "Sleepy saunter": 0.1,
            "celery saunter": 0.1
          },
          "sleep:exercise": {
            "Sleepy celery": 0.1,
            "Sleepy saunter": 0.8,
            "celery saunter": 0.1
          },
          "salad:exercise": {
            "Sleepy celery": 0.1,
            "Sleepy saunter": 0.1,
            "celery saunter": true
          }
        }
      }
    },
    "format": "long",
    "type": "binary",
    "studies": {
      "long": [
        {
          "study": "Sleepy celery",
          "id": 1,
          "t": "sleep",
          "r": "four",
          "n": 10,
          "rob": 1,
          "indirectness": 3
        },
        {
          "study": "Sleepy celery",
          "id": 1,
          "t": "salad",
          "r": 2,
          "n": 10,
          "rob": 1,
          "indirectness": 3
        },
        {
          "study": "Sleepy saunter",
          "id": 2,
          "t": "sleep",
          "r": 4,
          "n": 10,
          "rob": 2,
          "indirectness": 2
        },
        {
          "study": "Sleepy saunter",
          "id": 2,
          "t": "exercise",
          "r": 8,
          "n": 10,
          "rob": 2,
          "indirectness": 2
        },
        
        {
          "study": "celery saunter",
          "id": 3,
          "t": "salad",
          "r": 2,
          "n": 10,
          "rob": 3,
          "indirectness": 1
        },
        {
          "study": "celery saunter",
          "id": 3,
          "t": "exercise",
          "r": 8,
          "n": 10,
          "rob": 3,
          "indirectness": 1
        }
      ]
    }
  }
}
'

json_incorrect_types_errors <- data.frame() |>
  rbind(data.frame(field = "data.project.CM.contributionMatrices.hatmatrix.colNames", message = "is the wrong type")) |>
  rbind(data.frame(field = "data.project.CM.contributionMatrices.hatmatrix.colNamesNMAresults", message = "is the wrong type")) |>
  rbind(data.frame(field = "data.project.CM.contributionMatrices.hatmatrix.H.1.0", message = "is the wrong type")) |>
  rbind(data.frame(field = "data.project.CM.contributionMatrices.hatmatrix.model", message = "is the wrong type")) |>
  rbind(data.frame(field = "data.project.CM.contributionMatrices.hatmatrix.NMAresults.0.Direct", message = "is the wrong type")) |>
  rbind(data.frame(field = "data.project.CM.contributionMatrices.hatmatrix.NMAresults.0._row", message = "is the wrong type")) |>
  rbind(data.frame(field = "data.project.CM.contributionMatrices.hatmatrix.rowNames.2", message = "is the wrong type")) |>
  rbind(data.frame(field = "data.project.CM.contributionMatrices.hatmatrix.rowNamesNMAresults.2", message = "is the wrong type")) |>
  rbind(data.frame(field = "data.project.CM.contributionMatrices.hatmatrix.sm", message = "is the wrong type")) |>
  rbind(data.frame(field = "data.project.CM.contributionMatrices.studycontributions[keys3.2][keys5.2]", message = "is the wrong type")) |>
  rbind(data.frame(field = "data.project.studies.long.0.r", message = "is the wrong type"))

json_data_mixed_outcome_type <- '
{
  "project": {
    "CM": {
      "contributionMatrices": {
        "hatmatrix": {
          "colNames": [
            "sleep:salad",
            "sleep:exercise",
            "salad:exercise"
          ],
          "colNamesNMAresults": [
            "Direct",
            "DirectL",
            "DirectU",
            "Indirect",
            "IndirectL",
            "IndirectU",
            "SideIF",
            "SideIFlower",
            "SideIFupper",
            "SideZ",
            "SidePvalue",
            "PropDir",
            "NMA treatment effect",
            "se treat effect",
            "lower CI",
            "upper CI",
            "lower PrI",
            "upper PrI",
            "PropDirNetmeta",
            "_row"
          ],
          "H": [
            [ 0.1, 0.2, 0.3 ],
            [ 0.11, 0.22, 0.33 ],
            [ 0.111, 0.222, 0.333 ]
          ],
          "model": "fixed",
          "NMAresults": [
            {
              "Direct": -0.1,
              "DirectL": -0.1,
              "DirectU": -0.1,
              "Indirect": -1.1,
              "IndirectL": -1.1,
              "IndirectU": -1.1,
              "SideIF": 0.1,
              "SideIFlower": -0.1,
              "SideIFupper": 0.1,
              "SideZ": 0.1,
              "SidePvalue": 0.1,
              "PropDir": 0.1,
              "NMA treatment effect": -0.1,
              "se treat effect": 0.1,
              "lower CI": -0.1,
              "upper CI": -0.1,
              "lower PrI": -0.1,
              "upper PrI": -0.1,
              "PropDirNetmeta": 0.1,
              "_row": "sleep:salad"
            },
            {
              "Indirect": -2.2,
              "IndirectL": -2.2,
              "IndirectU": -2.2,
              "PropDir": 0.2,
              "NMA treatment effect": -0.2,
              "se treat effect": 0.2,
              "lower CI": -0.2,
              "upper CI": -0.2,
              "lower PrI": -0.2,
              "upper PrI": -0.2,
              "PropDirNetmeta": 0.2,
              "_row": "sleep:exercise"
            },
            {
              "Direct": -0.3,
              "DirectL": -0.3,
              "DirectU": -0.3,
              "PropDir": 0.3,
              "NMA treatment effect": -0.3,
              "se treat effect": 0.3,
              "lower CI": -0.3,
              "upper CI": -0.3,
              "lower PrI": -0.3,
              "upper PrI": -0.3,
              "PropDirNetmeta": 0.3,
              "_row": "salad:exercise"
            }
          ],
          "rowNames": [
            "sleep:salad",
            "sleep:exercise",
            "salad:exercise"
          ],
          "rowNamesNMAresults": [
            "sleep:salad",
            "sleep:exercise",
            "salad:exercise"
          ],
          "sm": "OR"
        },
        "studycontributions": {
          "sleep:salad": {
            "Sleepy celery": 0.8,
            "Sleepy saunter": 0.1,
            "celery saunter": 0.1
          },
          "sleep:exercise": {
            "Sleepy celery": 0.1,
            "Sleepy saunter": 0.8,
            "celery saunter": 0.1
          },
          "salad:exercise": {
            "Sleepy celery": 0.1,
            "Sleepy saunter": 0.1,
            "celery saunter": 0.8
          }
        }
      }
    },
    "format": "long",
    "type": "binary",
    "studies": {
      "long": [
        {
          "study": "Sleepy celery",
          "id": 1,
          "t": "sleep",
          "n": 10,
          "r": 4,
          "mean": 10,
          "rob": 1,
          "indirectness": 3
        },
        {
          "study": "Sleepy celery",
          "id": 1,
          "t": "salad",
          "n": 10,
          "r": 2,
          "mean": 10,
          "rob": 1,
          "indirectness": 3
        },
        {
          "study": "Sleepy saunter",
          "id": 2,
          "t": "sleep",
          "n": 10,
          "r": 4,
          "mean": 10,
          "rob": 2,
          "indirectness": 2
        },
        {
          "study": "Sleepy saunter",
          "id": 2,
          "t": "exercise",
          "n": 10,
          "r": 8,
          "mean": 10,
          "rob": 2,
          "indirectness": 2
        },
        
        {
          "study": "celery saunter",
          "id": 3,
          "t": "salad",
          "n": 10,
          "r": 2,
          "mean": 10,
          "rob": 3,
          "indirectness": 1
        },
        {
          "study": "celery saunter",
          "id": 3,
          "t": "exercise",
          "n": 10,
          "r": 8,
          "mean": 10,
          "rob": 3,
          "indirectness": 1
        }
      ]
    }
  }
}
'

json_data_mixed_outcome_type_errors <- data.frame() |>
  rbind(data.frame(field = "data.project.studies.long.0", message = "no (or more than one) schemas match")) |>
  rbind(data.frame(field = "data.project.studies.long.1", message = "no (or more than one) schemas match")) |>
  rbind(data.frame(field = "data.project.studies.long.2", message = "no (or more than one) schemas match")) |>
  rbind(data.frame(field = "data.project.studies.long.3", message = "no (or more than one) schemas match")) |>
  rbind(data.frame(field = "data.project.studies.long.4", message = "no (or more than one) schemas match")) |>
  rbind(data.frame(field = "data.project.studies.long.5", message = "no (or more than one) schemas match"))

json_data_both_outcome_type <- '
{
  "project": {
    "CM": {
      "contributionMatrices": {
        "hatmatrix": {
          "colNames": [
            "sleep:salad",
            "sleep:exercise",
            "salad:exercise"
          ],
          "colNamesNMAresults": [
            "Direct",
            "DirectL",
            "DirectU",
            "Indirect",
            "IndirectL",
            "IndirectU",
            "SideIF",
            "SideIFlower",
            "SideIFupper",
            "SideZ",
            "SidePvalue",
            "PropDir",
            "NMA treatment effect",
            "se treat effect",
            "lower CI",
            "upper CI",
            "lower PrI",
            "upper PrI",
            "PropDirNetmeta",
            "_row"
          ],
          "H": [
            [ 0.1, 0.2, 0.3 ],
            [ 0.11, 0.22, 0.33 ],
            [ 0.111, 0.222, 0.333 ]
          ],
          "model": "fixed",
          "NMAresults": [
            {
              "Direct": -0.1,
              "DirectL": -0.1,
              "DirectU": -0.1,
              "Indirect": -1.1,
              "IndirectL": -1.1,
              "IndirectU": -1.1,
              "SideIF": 0.1,
              "SideIFlower": -0.1,
              "SideIFupper": 0.1,
              "SideZ": 0.1,
              "SidePvalue": 0.1,
              "PropDir": 0.1,
              "NMA treatment effect": -0.1,
              "se treat effect": 0.1,
              "lower CI": -0.1,
              "upper CI": -0.1,
              "lower PrI": -0.1,
              "upper PrI": -0.1,
              "PropDirNetmeta": 0.1,
              "_row": "sleep:salad"
            },
            {
              "Indirect": -2.2,
              "IndirectL": -2.2,
              "IndirectU": -2.2,
              "PropDir": 0.2,
              "NMA treatment effect": -0.2,
              "se treat effect": 0.2,
              "lower CI": -0.2,
              "upper CI": -0.2,
              "lower PrI": -0.2,
              "upper PrI": -0.2,
              "PropDirNetmeta": 0.2,
              "_row": "sleep:exercise"
            },
            {
              "Direct": -0.3,
              "DirectL": -0.3,
              "DirectU": -0.3,
              "PropDir": 0.3,
              "NMA treatment effect": -0.3,
              "se treat effect": 0.3,
              "lower CI": -0.3,
              "upper CI": -0.3,
              "lower PrI": -0.3,
              "upper PrI": -0.3,
              "PropDirNetmeta": 0.3,
              "_row": "salad:exercise"
            }
          ],
          "rowNames": [
            "sleep:salad",
            "sleep:exercise",
            "salad:exercise"
          ],
          "rowNamesNMAresults": [
            "sleep:salad",
            "sleep:exercise",
            "salad:exercise"
          ],
          "sm": "OR"
        },
        "studycontributions": {
          "sleep:salad": {
            "Sleepy celery": 0.8,
            "Sleepy saunter": 0.1,
            "celery saunter": 0.1
          },
          "sleep:exercise": {
            "Sleepy celery": 0.1,
            "Sleepy saunter": 0.8,
            "celery saunter": 0.1
          },
          "salad:exercise": {
            "Sleepy celery": 0.1,
            "Sleepy saunter": 0.1,
            "celery saunter": 0.8
          }
        }
      }
    },
    "format": "long",
    "type": "binary",
    "studies": {
      "long": [
        {
          "study": "Sleepy celery",
          "id": 1,
          "t": "sleep",
          "r": 4,
          "n": 10,
          "mean": 10,
          "sd": 1,
          "rob": 1,
          "indirectness": 3
        },
        {
          "study": "Sleepy celery",
          "id": 1,
          "t": "salad",
          "r": 2,
          "n": 10,
          "mean": 10,
          "sd": 1,
          "rob": 1,
          "indirectness": 3
        },
        {
          "study": "Sleepy saunter",
          "id": 2,
          "t": "sleep",
          "r": 4,
          "n": 10,
          "mean": 10,
          "sd": 1,
          "rob": 2,
          "indirectness": 2
        },
        {
          "study": "Sleepy saunter",
          "id": 2,
          "t": "exercise",
          "r": 8,
          "n": 10,
          "mean": 10,
          "sd": 1,
          "rob": 2,
          "indirectness": 2
        },
        
        {
          "study": "celery saunter",
          "id": 3,
          "t": "salad",
          "r": 2,
          "n": 10,
          "mean": 10,
          "sd": 1,
          "rob": 3,
          "indirectness": 1
        },
        {
          "study": "celery saunter",
          "id": 3,
          "t": "exercise",
          "r": 8,
          "n": 10,
          "mean": 10,
          "sd": 1,
          "rob": 3,
          "indirectness": 1
        }
      ]
    }
  }
}
'

json_data_both_outcome_type_errors <- data.frame() |>
  rbind(data.frame(field = "data.project.studies.long.0", message = "no (or more than one) schemas match")) |>
  rbind(data.frame(field = "data.project.studies.long.1", message = "no (or more than one) schemas match")) |>
  rbind(data.frame(field = "data.project.studies.long.2", message = "no (or more than one) schemas match")) |>
  rbind(data.frame(field = "data.project.studies.long.3", message = "no (or more than one) schemas match")) |>
  rbind(data.frame(field = "data.project.studies.long.4", message = "no (or more than one) schemas match")) |>
  rbind(data.frame(field = "data.project.studies.long.5", message = "no (or more than one) schemas match"))

json_missing_fields <- '
{
  "project": {
    "CM": {
      "contributionMatrices": {
        "hatmatrix": {
          "colNames": [
            "sleep:salad",
            "sleep:exercise",
            "salad:exercise"
          ],
          "colNamesNMAresults": [
            "Direct",
            "DirectL",
            "DirectU",
            "Indirect",
            "IndirectL",
            "IndirectU",
            "SideIF",
            "SideIFlower",
            "SideIFupper",
            "SideZ",
            "SidePvalue",
            "PropDir",
            "NMA treatment effect",
            "se treat effect",
            "lower CI",
            "upper CI",
            "lower PrI",
            "upper PrI",
            "PropDirNetmeta",
            "_row"
          ],
          "model": "fixed",
          "NMAresults": [
            {
              "Direct": -0.1,
              "DirectL": -0.1,
              "DirectU": -0.1,
              "Indirect": -1.1,
              "IndirectL": -1.1,
              "IndirectU": -1.1,
              "SideIF": 0.1,
              "SideIFlower": -0.1,
              "SideIFupper": 0.1,
              "SideZ": 0.1,
              "SidePvalue": 0.1,
              "PropDir": 0.1,
              "NMA treatment effect": -0.1,
              "se treat effect": 0.1,
              "lower CI": -0.1,
              "upper CI": -0.1,
              "lower PrI": -0.1,
              "upper PrI": -0.1,
              "PropDirNetmeta": 0.1,
              "_row": "sleep:salad"
            },
            {
              "Indirect": -2.2,
              "IndirectL": -2.2,
              "IndirectU": -2.2,
              "PropDir": 0.2,
              "NMA treatment effect": -0.2,
              "se treat effect": 0.2,
              "lower CI": -0.2,
              "upper CI": -0.2,
              "lower PrI": -0.2,
              "upper PrI": -0.2,
              "PropDirNetmeta": 0.2
            },
            {
              "Direct": -0.3,
              "DirectL": -0.3,
              "DirectU": -0.3,
              "PropDir": 0.3,
              "NMA treatment effect": -0.3,
              "se treat effect": 0.3,
              "lower CI": -0.3,
              "upper CI": -0.3,
              "lower PrI": -0.3,
              "upper PrI": -0.3,
              "PropDirNetmeta": 0.3,
              "_row": "salad:exercise"
            }
          ],
          "rowNames": [
            "sleep:salad",
            "sleep:exercise",
            "salad:exercise"
          ],
          "rowNamesNMAresults": [
            "sleep:salad",
            "sleep:exercise",
            "salad:exercise"
          ],
          "sm": "OR"
        },
        "studycontributions": {
          "sleep:salad": {
            "Sleepy celery": 0.8,
            "Sleepy saunter": 0.1,
            "celery saunter": 0.1
          },
          "sleep:exercise": {
            "Sleepy celery": 0.1,
            "Sleepy saunter": 0.8,
            "celery saunter": 0.1
          },
          "salad:exercise": {
            "Sleepy celery": 0.1,
            "Sleepy saunter": 0.1,
            "celery saunter": 0.8
          }
        }
      }
    },
    "format": "long",
    "type": "binary",
    "studies": {
      "long": [
        {
          "id": 1,
          "t": "sleep",
          "r": 4,
          "n": 10,
          "rob": 1,
          "indirectness": 3
        },
        {
          "study": "Sleepy celery",
          "t": "salad",
          "r": 2,
          "n": 10,
          "rob": 1,
          "indirectness": 3
        },
        {
          "study": "Sleepy saunter",
          "id": 2,
          "r": 4,
          "n": 10,
          "rob": 2,
          "indirectness": 2
        },
        {
          "study": "Sleepy saunter",
          "id": 2,
          "t": "exercise",
          "n": 10,
          "rob": 2,
          "indirectness": 2
        },
        {
          "study": "celery saunter",
          "id": 3,
          "t": "salad",
          "r": 2,
          "rob": 3,
          "indirectness": 1
        },
        {
          "study": "celery saunter",
          "id": 3,
          "t": "exercise",
          "r": 8,
          "n": 10
        }
      ]
    }
  }
}
'

json_missing_fields_errors <- data.frame() |>
  rbind(data.frame(field = "data.project.CM.contributionMatrices.hatmatrix.H", message = "is required")) |>
  rbind(data.frame(field = "data.project.CM.contributionMatrices.hatmatrix.NMAresults.1._row", message = "is required")) |>
  rbind(data.frame(field = "data.project.studies.long.0.study", message = "is required")) |>
  rbind(data.frame(field = "data.project.studies.long.1.id", message = "is required")) |>
  rbind(data.frame(field = "data.project.studies.long.2.t", message = "is required")) |>
  rbind(data.frame(field = "data.project.studies.long.3", message = "no (or more than one) schemas match")) |>
  rbind(data.frame(field = "data.project.studies.long.4.n", message = "is required")) |>
  rbind(data.frame(field = "data.project.studies.long.5.rob", message = "is required")) |>
  rbind(data.frame(field = "data.project.studies.long.5.indirectness", message = "is required"))

json_values_out_of_bounds <- '
{
  "project": {
    "CM": {
      "contributionMatrices": {
        "hatmatrix": {
          "colNames": [
            "sleep-salad",
            "sleep:exercise",
            "salad:exercise"
          ],
          "colNamesNMAresults": [
            "SMELLY",
            "DirectL",
            "DirectU",
            "Indirect",
            "IndirectL",
            "IndirectU",
            "SideIF",
            "SideIFlower",
            "SideIFupper",
            "SideZ",
            "SidePvalue",
            "PropDir",
            "NMA treatment effect",
            "se treat effect",
            "lower CI",
            "upper CI",
            "lower PrI",
            "upper PrI",
            "PropDirNetmeta",
            "_row"
          ],
          "H": [
            [ -0.1, 0.2, 0.3 ],
            [ 0.11, 0.22, 0.33 ],
            [ 0.111, 0.222, 0.333 ]
          ],
          "NMAresults": [
            {
              "Direct": -0.1,
              "DirectL": -0.1,
              "DirectU": -0.1,
              "Indirect": -1.1,
              "IndirectL": -1.1,
              "IndirectU": -1.1,
              "SideIF": 0.1,
              "SideIFlower": -0.1,
              "SideIFupper": 0.1,
              "SideZ": 0.1,
              "SidePvalue": 0.1,
              "PropDir": 0.1,
              "NMA treatment effect": -0.1,
              "se treat effect": 0.1,
              "lower CI": -0.1,
              "upper CI": -0.1,
              "lower PrI": -0.1,
              "upper PrI": -0.1,
              "PropDirNetmeta": 0.1,
              "_row": "sleep:salad"
            },
            {
              "Indirect": -2.2,
              "IndirectL": -2.2,
              "IndirectU": -2.2,
              "PropDir": 0.2,
              "NMA treatment effect": -0.2,
              "se treat effect": 0.2,
              "lower CI": -0.2,
              "upper CI": -0.2,
              "lower PrI": -0.2,
              "upper PrI": -0.2,
              "PropDirNetmeta": 0.2,
              "_row": "sleep:exercise"
            },
            {
              "Direct": -0.3,
              "DirectL": -0.3,
              "DirectU": -0.3,
              "PropDir": 0.3,
              "NMA treatment effect": -0.3,
              "se treat effect": 0.3,
              "lower CI": -0.3,
              "upper CI": -0.3,
              "lower PrI": -0.3,
              "upper PrI": -0.3,
              "PropDirNetmeta": 0.3,
              "_row": "salad-exercise"
            }
          ],
          "model": "pseudo-random",
          "rowNames": [
            "sleep-salad",
            "sleep:exercise",
            "salad:exercise"
          ],
          "rowNamesNMAresults": [
            "sleep:salad",
            "sleep-exercise",
            "salad:exercise"
          ],
          "sm": "Evens-Ratio"
        },
        "studycontributions": {
          "sleep:salad": {
            "Sleepy celery": -0.8,
            "Sleepy saunter": -0.1,
            "celery saunter": -0.1
          },
          "sleep:exercise": {
            "Sleepy-celery": 0.1,
            "Sleepy saunter": 0.8,
            "celery saunter": 0.1
          },
          "salad vs exercise": {
            "Sleepy celery": 0.1,
            "Sleepy saunter": 0.1,
            "celery saunter": 0.8
          }
        }
      }
    },
    "format": "longish",
    "type": "strawberry",
    "studies": {
      "long": [
        {
          "study": "Sleepy celery",
          "id": 0,
          "t": "salad",
          "r": 4,
          "n": 10,
          "rob": 1,
          "indirectness": 3
        },
        {
          "study": "Sleepy celery",
          "id": 1,
          "t": "",
          "r": 2,
          "n": 10,
          "rob": 1,
          "indirectness": 3
        },
        {
          "study": "Sleepy saunter",
          "id": 2,
          "t": "sleep",
          "r": -1,
          "n": 10,
          "rob": 2,
          "indirectness": 2
        },
        {
          "study": "Sleepy saunter",
          "id": 2,
          "t": "exercise",
          "r": 8,
          "n": 0,
          "rob": 2,
          "indirectness": 2
        },
        
        {
          "study": "celery saunter",
          "id": 3,
          "t": "salad",
          "r": 2,
          "n": 10,
          "rob": 30,
          "indirectness": 1
        },
        {
          "study": "celery saunter",
          "id": 3,
          "t": "exercise",
          "r": 8,
          "n": 10,
          "rob": 3,
          "indirectness": 100
        }
      ]
    }
  }
}
'

json_values_out_of_bounds_errors <- data.frame() |>
  rbind(data.frame(field = "data.project.CM.contributionMatrices.hatmatrix.colNames.0", message = "pattern mismatch")) |>
  rbind(data.frame(field = "data.project.CM.contributionMatrices.hatmatrix.colNamesNMAresults.0", message = "must be an enum value")) |>
  rbind(data.frame(field = "data.project.CM.contributionMatrices.hatmatrix.model", message = "must be an enum value")) |>
  rbind(data.frame(field = "data.project.CM.contributionMatrices.hatmatrix.NMAresults.2._row", message = "pattern mismatch")) |>
  rbind(data.frame(field = "data.project.CM.contributionMatrices.hatmatrix.rowNames.0", message = "pattern mismatch")) |>
  rbind(data.frame(field = "data.project.CM.contributionMatrices.hatmatrix.rowNamesNMAresults.1", message = "pattern mismatch")) |>
  rbind(data.frame(field = "data.project.CM.contributionMatrices.hatmatrix.sm", message = "must be an enum value")) |>
  rbind(data.frame(field = "data.project.CM.contributionMatrices.studycontributions", message = "has additional properties")) |>
  rbind(data.frame(field = "data.project.CM.contributionMatrices.studycontributions[keys3.1]", message = "has additional properties")) |>
  rbind(data.frame(field = "data.project.format", message = "must be an enum value")) |>
  rbind(data.frame(field = "data.project.type", message = "must be an enum value")) |>
  rbind(data.frame(field = "data.project.studies.long.0.id", message = "is less than minimum")) |>
  rbind(data.frame(field = "data.project.studies.long.1.t", message = "pattern mismatch")) |>
  rbind(data.frame(field = "data.project.studies.long.2.r", message = "is less than minimum")) |>
  rbind(data.frame(field = "data.project.studies.long.3.n", message = "is less than minimum")) |>
  rbind(data.frame(field = "data.project.studies.long.4.rob", message = "is more than maximum")) |>
  rbind(data.frame(field = "data.project.studies.long.5.indirectness", message = "is more than maximum"))

json_short_arrays <- '
{
  "project": {
    "CM": {
      "contributionMatrices": {
        "hatmatrix": {
          "colNames": [],
          "colNamesNMAresults": [],
          "H": [],
          "model": "fixed",
          "NMAresults": [],
          "rowNames": [],
          "rowNamesNMAresults": [],
          "sm": "OR"
        },
        "studycontributions": {
          "sleep:salad": {
            "Sleepy celery": 0.8,
            "Sleepy saunter": 0.1,
            "celery saunter": 0.1
          },
          "sleep:exercise": {
            "Sleepy celery": 0.1,
            "Sleepy saunter": 0.8,
            "celery saunter": 0.1
          },
          "salad:exercise": {
            "Sleepy celery": 0.1,
            "Sleepy saunter": 0.1,
            "celery saunter": 0.8
          }
        }
      }
    },
    "format": "long",
    "type": "binary",
    "studies": {
      "long": [
        {
          "study": "Sleepy celery",
          "id": 1,
          "t": "sleep",
          "r": 4,
          "n": 10,
          "rob": 1,
          "indirectness": 3
        }
      ]
    }
  }
}
'

json_short_arrays_errors <- data.frame() |>
  rbind(data.frame(field = "data.project.CM.contributionMatrices.hatmatrix.colNames", message = "has less items than allowed")) |>
  rbind(data.frame(field = "data.project.CM.contributionMatrices.hatmatrix.colNamesNMAresults", message = "has less items than allowed")) |>
  rbind(data.frame(field = "data.project.CM.contributionMatrices.hatmatrix.H", message = "has less items than allowed")) |>
  rbind(data.frame(field = "data.project.CM.contributionMatrices.hatmatrix.NMAresults", message = "has less items than allowed")) |>
  rbind(data.frame(field = "data.project.CM.contributionMatrices.hatmatrix.rowNames", message = "has less items than allowed")) |>
  rbind(data.frame(field = "data.project.CM.contributionMatrices.hatmatrix.rowNamesNMAresults", message = "has less items than allowed")) |>
  rbind(data.frame(field = "data.project.studies.long", message = "has less items than allowed"))

json_duplicate_row_column_names <- '
{
  "project": {
    "CM": {
      "contributionMatrices": {
        "hatmatrix": {
          "colNames": [
            "sleep:salad",
            "sleep:salad",
            "sleep:exercise",
            "salad:exercise"
          ],
          "colNamesNMAresults": [
            "Direct",
            "Direct",
            "DirectU",
            "Indirect",
            "IndirectL",
            "IndirectU",
            "SideIF",
            "SideIFlower",
            "SideIFupper",
            "SideZ",
            "SidePvalue",
            "PropDir",
            "NMA treatment effect",
            "se treat effect",
            "lower CI",
            "upper CI",
            "lower PrI",
            "upper PrI",
            "PropDirNetmeta",
            "_row"
          ],
          "H": [
            [ 0.1, 0.2, 0.3 ],
            [ 0.11, 0.22, 0.33 ],
            [ 0.111, 0.222, 0.333 ]
          ],
          "model": "fixed",
          "NMAresults": [
            {
              "Direct": -0.1,
              "DirectL": -0.1,
              "DirectU": -0.1,
              "Indirect": -1.1,
              "IndirectL": -1.1,
              "IndirectU": -1.1,
              "SideIF": 0.1,
              "SideIFlower": -0.1,
              "SideIFupper": 0.1,
              "SideZ": 0.1,
              "SidePvalue": 0.1,
              "PropDir": 0.1,
              "NMA treatment effect": -0.1,
              "se treat effect": 0.1,
              "lower CI": -0.1,
              "upper CI": -0.1,
              "lower PrI": -0.1,
              "upper PrI": -0.1,
              "PropDirNetmeta": 0.1,
              "_row": "sleep:salad"
            },
            {
              "Indirect": -2.2,
              "IndirectL": -2.2,
              "IndirectU": -2.2,
              "PropDir": 0.2,
              "NMA treatment effect": -0.2,
              "se treat effect": 0.2,
              "lower CI": -0.2,
              "upper CI": -0.2,
              "lower PrI": -0.2,
              "upper PrI": -0.2,
              "PropDirNetmeta": 0.2,
              "_row": "sleep:exercise"
            },
            {
              "Direct": -0.3,
              "DirectL": -0.3,
              "DirectU": -0.3,
              "PropDir": 0.3,
              "NMA treatment effect": -0.3,
              "se treat effect": 0.3,
              "lower CI": -0.3,
              "upper CI": -0.3,
              "lower PrI": -0.3,
              "upper PrI": -0.3,
              "PropDirNetmeta": 0.3,
              "_row": "salad:exercise"
            }
          ],
          "rowNames": [
            "sleep:salad",
            "sleep:salad",
            "sleep:exercise",
            "salad:exercise"
          ],
          "rowNamesNMAresults": [
            "sleep:salad",
            "sleep:salad",
            "sleep:exercise",
            "salad:exercise"
          ],
          "sm": "OR"
        },
        "studycontributions": {
          "sleep:salad": {
            "Sleepy celery": 0.8,
            "Sleepy saunter": 0.1,
            "celery saunter": 0.1
          },
          "sleep:exercise": {
            "Sleepy celery": 0.1,
            "Sleepy saunter": 0.8,
            "celery saunter": 0.1
          },
          "salad:exercise": {
            "Sleepy celery": 0.1,
            "Sleepy saunter": 0.1,
            "celery saunter": 0.8
          }
        }
      }
    },
    "format": "long",
    "type": "binary",
    "studies": {
      "long": [
        {
          "study": "Sleepy celery",
          "id": 1,
          "t": "sleep",
          "r": 4,
          "n": 10,
          "rob": 1,
          "indirectness": 3
        },
        {
          "study": "Sleepy celery",
          "id": 1,
          "t": "salad",
          "r": 2,
          "n": 10,
          "rob": 1,
          "indirectness": 3
        },
        {
          "study": "Sleepy saunter",
          "id": 2,
          "t": "sleep",
          "r": 4,
          "n": 10,
          "rob": 2,
          "indirectness": 2
        },
        {
          "study": "Sleepy saunter",
          "id": 2,
          "t": "exercise",
          "r": 8,
          "n": 10,
          "rob": 2,
          "indirectness": 2
        },
        
        {
          "study": "celery saunter",
          "id": 3,
          "t": "salad",
          "r": 2,
          "n": 10,
          "rob": 3,
          "indirectness": 1
        },
        {
          "study": "celery saunter",
          "id": 3,
          "t": "exercise",
          "r": 8,
          "n": 10,
          "rob": 3,
          "indirectness": 1
        }
      ]
    }
  }
}
'

json_duplicate_row_column_names_errors <- data.frame() |>
  rbind(data.frame(field = "data.project.CM.contributionMatrices.hatmatrix.colNames", message = "must be unique")) |>
  rbind(data.frame(field = "data.project.CM.contributionMatrices.hatmatrix.colNamesNMAresults", message = "must be unique")) |>
  rbind(data.frame(field = "data.project.CM.contributionMatrices.hatmatrix.rowNames", message = "must be unique")) |>
  rbind(data.frame(field = "data.project.CM.contributionMatrices.hatmatrix.rowNamesNMAresults", message = "must be unique"))

json_empty_strings <- '
{
  "project": {
    "CM": {
      "contributionMatrices": {
        "hatmatrix": {
          "colNames": [
            "",
            "sleep:exercise",
            "salad:exercise"
          ],
          "colNamesNMAresults": [
            "",
            "DirectL",
            "DirectU",
            "Indirect",
            "IndirectL",
            "IndirectU",
            "SideIF",
            "SideIFlower",
            "SideIFupper",
            "SideZ",
            "SidePvalue",
            "PropDir",
            "NMA treatment effect",
            "se treat effect",
            "lower CI",
            "upper CI",
            "lower PrI",
            "upper PrI",
            "PropDirNetmeta",
            "_row"
          ],
          "H": [
            [ 0.1, 0.2, 0.3 ],
            [ 0.11, 0.22, 0.33 ],
            [ 0.111, 0.222, 0.333 ]
          ],
          "model": "",
          "NMAresults": [
            {
              "Direct": -0.1,
              "DirectL": -0.1,
              "DirectU": -0.1,
              "Indirect": -1.1,
              "IndirectL": -1.1,
              "IndirectU": -1.1,
              "SideIF": 0.1,
              "SideIFlower": -0.1,
              "SideIFupper": 0.1,
              "SideZ": 0.1,
              "SidePvalue": 0.1,
              "PropDir": 0.1,
              "NMA treatment effect": -0.1,
              "se treat effect": 0.1,
              "lower CI": -0.1,
              "upper CI": -0.1,
              "lower PrI": -0.1,
              "upper PrI": -0.1,
              "PropDirNetmeta": 0.1,
              "_row": ""
            },
            {
              "Indirect": -2.2,
              "IndirectL": -2.2,
              "IndirectU": -2.2,
              "PropDir": 0.2,
              "NMA treatment effect": -0.2,
              "se treat effect": 0.2,
              "lower CI": -0.2,
              "upper CI": -0.2,
              "lower PrI": -0.2,
              "upper PrI": -0.2,
              "PropDirNetmeta": 0.2,
              "_row": "sleep:exercise"
            },
            {
              "Direct": -0.3,
              "DirectL": -0.3,
              "DirectU": -0.3,
              "PropDir": 0.3,
              "NMA treatment effect": -0.3,
              "se treat effect": 0.3,
              "lower CI": -0.3,
              "upper CI": -0.3,
              "lower PrI": -0.3,
              "upper PrI": -0.3,
              "PropDirNetmeta": 0.3,
              "_row": "salad:exercise"
            }
          ],
          "rowNames": [
            "",
            "sleep:exercise",
            "salad:exercise"
          ],
          "rowNamesNMAresults": [
            "sleep:salad",
            "",
            "salad:exercise"
          ],
          "sm": ""
        },
        "studycontributions": {
          "sleep:salad": {
            "Sleepy celery": 0.8,
            "Sleepy saunter": 0.1,
            "celery saunter": 0.1
          },
          "sleep:exercise": {
            "Sleepy celery": 0.1,
            "Sleepy saunter": 0.8,
            "celery saunter": 0.1
          },
          "salad:exercise": {
            "Sleepy celery": 0.1,
            "Sleepy saunter": 0.1,
            "celery saunter": 0.8
          }
        }
      }
    },
    "format": "",
    "type": "",
    "studies": {
      "long": [
        {
          "study": "",
          "id": 1,
          "t": "",
          "r": 4,
          "n": 10,
          "rob": 1,
          "indirectness": 3
        },
        {
          "study": "Sleepy celery",
          "id": 1,
          "t": "salad",
          "r": 2,
          "n": 10,
          "rob": 1,
          "indirectness": 3
        },
        {
          "study": "Sleepy saunter",
          "id": 2,
          "t": "sleep",
          "r": 4,
          "n": 10,
          "rob": 2,
          "indirectness": 2
        },
        {
          "study": "Sleepy saunter",
          "id": 2,
          "t": "exercise",
          "r": 8,
          "n": 10,
          "rob": 2,
          "indirectness": 2
        },
        
        {
          "study": "celery saunter",
          "id": 3,
          "t": "salad",
          "r": 2,
          "n": 10,
          "rob": 3,
          "indirectness": 1
        },
        {
          "study": "celery saunter",
          "id": 3,
          "t": "exercise",
          "r": 8,
          "n": 10,
          "rob": 3,
          "indirectness": 1
        }
      ]
    }
  }
}
'

json_empty_strings_errors <- data.frame() |>
  rbind(data.frame(field = "data.project.CM.contributionMatrices.hatmatrix.colNames.0", message = "pattern mismatch")) |>
  rbind(data.frame(field = "data.project.CM.contributionMatrices.hatmatrix.colNamesNMAresults.0", message = "must be an enum value")) |>
  rbind(data.frame(field = "data.project.CM.contributionMatrices.hatmatrix.model", message = "must be an enum value")) |>
  rbind(data.frame(field = "data.project.CM.contributionMatrices.hatmatrix.NMAresults.0._row", message = "pattern mismatch")) |>
  rbind(data.frame(field = "data.project.CM.contributionMatrices.hatmatrix.rowNames.0", message = "pattern mismatch")) |>
  rbind(data.frame(field = "data.project.CM.contributionMatrices.hatmatrix.rowNamesNMAresults.1", message = "pattern mismatch")) |>
  rbind(data.frame(field = "data.project.CM.contributionMatrices.hatmatrix.sm", message = "must be an enum value")) |>
  rbind(data.frame(field = "data.project.format", message = "must be an enum value")) |>
  rbind(data.frame(field = "data.project.type", message = "must be an enum value")) |>
  rbind(data.frame(field = "data.project.studies.long.0.study", message = "pattern mismatch")) |>
  rbind(data.frame(field = "data.project.studies.long.0.t", message = "pattern mismatch"))

json_mismatched_direct_or_indirect <- '
{
  "project": {
    "CM": {
      "contributionMatrices": {
        "hatmatrix": {
          "colNames": [
            "sleep:salad",
            "sleep:exercise",
            "salad:exercise"
          ],
          "colNamesNMAresults": [
            "Direct",
            "DirectL",
            "DirectU",
            "Indirect",
            "IndirectL",
            "IndirectU",
            "SideIF",
            "SideIFlower",
            "SideIFupper",
            "SideZ",
            "SidePvalue",
            "PropDir",
            "NMA treatment effect",
            "se treat effect",
            "lower CI",
            "upper CI",
            "lower PrI",
            "upper PrI",
            "PropDirNetmeta",
            "_row"
          ],
          "H": [
            [ 0.1, 0.2, 0.3 ],
            [ 0.11, 0.22, 0.33 ],
            [ 0.111, 0.222, 0.333 ]
          ],
          "model": "fixed",
          "NMAresults": [
            {
              "Direct": -0.1,
              "DirectL": -0.1,
              "DirectU": -0.1,
              "Indirect": -1.1,
              "IndirectL": -1.1,
              "IndirectU": -1.1,
              "PropDir": 0.1,
              "NMA treatment effect": -0.1,
              "se treat effect": 0.1,
              "lower CI": -0.1,
              "upper CI": -0.1,
              "lower PrI": -0.1,
              "upper PrI": -0.1,
              "PropDirNetmeta": 0.1,
              "_row": "sleep:salad"
            },
            {
              "Indirect": -2.2,
              "IndirectL": -2.2,
              "IndirectU": -2.2,
              "SideIF": 0.1,
              "SideIFlower": -0.1,
              "SideIFupper": 0.1,
              "SideZ": 0.1,
              "SidePvalue": 0.1,
              "PropDir": 0.2,
              "NMA treatment effect": -0.2,
              "se treat effect": 0.2,
              "lower CI": -0.2,
              "upper CI": -0.2,
              "lower PrI": -0.2,
              "upper PrI": -0.2,
              "PropDirNetmeta": 0.2,
              "_row": "sleep:exercise"
            },
            {
              "Direct": -0.3,
              "DirectL": -0.3,
              "DirectU": -0.3,
              "SideIF": 0.1,
              "SideIFlower": -0.1,
              "SideIFupper": 0.1,
              "SideZ": 0.1,
              "SidePvalue": 0.1,
              "PropDir": 0.3,
              "NMA treatment effect": -0.3,
              "se treat effect": 0.3,
              "lower CI": -0.3,
              "upper CI": -0.3,
              "lower PrI": -0.3,
              "upper PrI": -0.3,
              "PropDirNetmeta": 0.3,
              "_row": "salad:exercise"
            }
          ],
          "rowNames": [
            "sleep:salad",
            "sleep:exercise",
            "salad:exercise"
          ],
          "rowNamesNMAresults": [
            "sleep:salad",
            "sleep:exercise",
            "salad:exercise"
          ],
          "sm": "OR"
        },
        "studycontributions": {
          "sleep:salad": {
            "Sleepy celery": 0.8,
            "Sleepy saunter": 0.1,
            "celery saunter": 0.1
          },
          "sleep:exercise": {
            "Sleepy celery": 0.1,
            "Sleepy saunter": 0.8,
            "celery saunter": 0.1
          },
          "salad:exercise": {
            "Sleepy celery": 0.1,
            "Sleepy saunter": 0.1,
            "celery saunter": 0.8
          }
        }
      }
    },
    "format": "long",
    "type": "binary",
    "studies": {
      "long": [
        {
          "study": "Sleepy celery",
          "id": 1,
          "t": "sleep",
          "r": 4,
          "n": 10,
          "rob": 1,
          "indirectness": 3
        },
        {
          "study": "Sleepy celery",
          "id": 1,
          "t": "salad",
          "r": 2,
          "n": 10,
          "rob": 1,
          "indirectness": 3
        },
        {
          "study": "Sleepy saunter",
          "id": 2,
          "t": "sleep",
          "r": 4,
          "n": 10,
          "rob": 2,
          "indirectness": 2
        },
        {
          "study": "Sleepy saunter",
          "id": 2,
          "t": "exercise",
          "r": 8,
          "n": 10,
          "rob": 2,
          "indirectness": 2
        },
        
        {
          "study": "celery saunter",
          "id": 3,
          "t": "salad",
          "r": 2,
          "n": 10,
          "rob": 3,
          "indirectness": 1
        },
        {
          "study": "celery saunter",
          "id": 3,
          "t": "exercise",
          "r": 8,
          "n": 10,
          "rob": 3,
          "indirectness": 1
        }
      ]
    }
  }
}
'

json_mismatched_direct_or_indirect_errors <- data.frame() |>
  rbind(data.frame(field = "data.project.CM.contributionMatrices.hatmatrix.NMAresults.0", message = "no (or more than one) schemas match")) |>
  rbind(data.frame(field = "data.project.CM.contributionMatrices.hatmatrix.NMAresults.1", message = "no (or more than one) schemas match")) |>
  rbind(data.frame(field = "data.project.CM.contributionMatrices.hatmatrix.NMAresults.2", message = "no (or more than one) schemas match"))
