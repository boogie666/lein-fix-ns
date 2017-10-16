(ns leiningen.fix-ns
  (:require [clojure.tools.namespace.file :as file]
            [clojure.tools.namespace.dir :as dir]
            [clojure.java.io :as io]
            [clojure.string :as str]))


(defn correct-ns [src-root f]
  (let [path (.getPath f)]
    (-> path
        (str/replace-first (re-pattern (str src-root java.io.File/separator)) "")
        (str/replace #"\.cljs" "")
        (str/replace #"/" ".")
        (str/replace #"_" "-")
        symbol)))

(defn correct-file-name [f]
  (let [path (.getPath f)]
    (-> path
        (str/replace #"-" "_"))))

(defn ns-structure [src-root file]
  (let [ns-declaration (file/read-file-ns-decl file)
        ns-name (second ns-declaration)
        source (slurp file)
        correct-ns-name (correct-ns src-root file)
        correct-file-name (correct-file-name file)]
    {:name ns-name
     :correct-name correct-ns-name
     :file file
     :correct-file-name correct-file-name
     :source source}))

(defn xform [src-root ext]
  (comp (filter #(.isFile %))
     (filter #(.endsWith (.getName %) ext))
     (map (partial ns-structure src-root))))

(def symbol-regex #"[a-zA-Z0-9$%*+=?!<>_-]['.a-zA-Z0-9$%*+=?!<>_-]*")

(defn symbol-map [source-data]
  (reduce (fn [result data]
            (assoc result (str (:name data)) data))
          {}
          source-data))

(defn correct-requires [source-data]
  (let [symbol-map (symbol-map source-data)]
    (map (fn [source]
          (assoc source :correct-source
                (str/replace (:source source)
                             symbol-regex
                             #(str (get-in symbol-map [% :correct-name] %)))))
         source-data)))


(defn write-source! [source-data]
  (doseq [s source-data]
    (let [{:keys [correct-source file correct-file-name]} s]
      (io/delete-file file true)
      (io/make-parents correct-file-name)
      (spit correct-file-name correct-source))))


(defn source-data! [ext src-root]
  (let [root (io/file src-root)]
    (into [] (xform src-root ext) (file-seq root))))

(defn move-files-correctly! [ext src-root]
  (let [source-data (source-data! ext src-root)
        fixed-source (correct-requires source-data)]
    (write-source! fixed-source)
    (println (str "finished with " src-root " on ." ext " files"))))


(defn fix-ns
  "fixed namespaces based on folder location"
  [project & args]
  (let [{:keys [source-paths fix-ns-config] :or {fix-ns-config {:ext "clj"}}} project]
    (doseq [src-root source-paths]
      (move-files-correctly! (:ext fix-ns-config) src-root))))

