(in-package :om)

(defvar *oracles* nil)

(setq *oracles*
      '("AutumnleavesDoMin-solo1-Chan9-2014.3.1-13h40.or"
        "AutumnleavesDoMin-solo2-Chan10-2014.3.1-13h40.or"
        "AutumnleavesDoMin-solo3-Chan11-2014.3.1-13h40.or"
        "AutumnleavesDoMin-walkingChan3-2014.2.27-22h54.or"
        "Blues-solo.or"
        "Blues-1Normal.or"
        "Blues-2Fast.or"
        "JSantosNeto-Balaio.or"
        "Balaio_Solo-Jovino.or"
        "Brassens.or"
        "Dicidenbas-solo4tes.or"
        "VoicingsHermeto.or"
        ;;;JNMR
        "Jaime_solo.or"
        "Jaime-24fev13-solo1.or"
        "Jaime-24fev13-solo2.or"
        "Jaime.melo-juil11.or"
        "JaimeSolo2-nov11.or"
        "Jaime_accomp.or"
        ))

(setq *db-path-solo1* (namestring (decode-local-path (nth 0 *oracles*)))
      *db-path-solo2* (namestring (decode-local-path (nth 1 *oracles*)))
      *db-path-solo3* (namestring (decode-local-path (nth 2 *oracles*)))
      *db-path-accomp1* (namestring (decode-local-path (nth 3 *oracles*)))
      *db-path-soloblues1* (namestring (decode-local-path (nth 4 *oracles*)))
      *db-path-soloblues2* (namestring (decode-local-path (nth 5 *oracles*)))
      *db-path-soloblues3* (namestring (decode-local-path (nth 6 *oracles*)))
      *db-path-JovinoSantosNeto1* (namestring (decode-local-path (nth 7 *oracles*)))
      *db-path-JovinoSantosNeto2* (namestring (decode-local-path (nth 8 *oracles*)))
      *db-path-Brassens* (namestring (decode-local-path (nth 9 *oracles*)))
      *db-path-Dicidenbas_Lubat* (namestring (decode-local-path (nth 10 *oracles*)))
      *db-path-accomp2* (namestring (decode-local-path (nth 11 *oracles*)))
      ;;;JNMR
      *db-path-jaimesolo1* (namestring (decode-local-path (nth 12 *oracles*)))
      *db-path-jaimesolo2* (namestring (decode-local-path (nth 13 *oracles*)))
      *db-path-jaimesolo3* (namestring (decode-local-path (nth 14 *oracles*)))
      *db-path-jaimesolo4* (namestring (decode-local-path (nth 15 *oracles*)))
      *db-path-jaimesolo5* (namestring (decode-local-path (nth 16 *oracles*)))
      *db-path-jaimeaccomp* (namestring (decode-local-path (nth 17 *oracles*)))
      )