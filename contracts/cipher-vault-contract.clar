;; cipher-vault-architecture
;; Enables secure registration, modification, and permission-based access to unique blockchain artifacts

;; -----------------------------
;; CORE PROTOCOL CONSTANTS
;; -----------------------------

;; Error response mechanisms for protocol operations
(define-constant ERR-ARTIFACT-NOT-FOUND (err u301))
(define-constant ERR-UNAUTHORIZED-ACCESS (err u305))
(define-constant ERR-ADMIN-ONLY-OPERATION (err u307))
(define-constant ERR-DUPLICATE-REGISTRATION (err u302))
(define-constant ERR-INVALID-IDENTIFIER-FORMAT (err u303))
(define-constant ERR-DIMENSION-OUT-OF-BOUNDS (err u304))
(define-constant ERR-INVALID-RECIPIENT (err u306))
(define-constant ERR-VIEW-PERMISSION-DENIED (err u308))

;; Protocol administrator designation
(define-constant PROTOCOL-CONTROLLER tx-sender)

;; -----------------------------
;; PERSISTENT STATE VARIABLES
;; -----------------------------

;; Global counter for tracking registered digital artifacts
(define-data-var total-registered-artifacts uint u0)

;; -----------------------------
;; AUXILIARY VALIDATION ROUTINES
;; -----------------------------

;; Determines if artifact identifier exists within the vault
(define-private (artifact-registered? (artifact-identifier uint))
  (is-some (map-get? vault-artifact-storage { artifact-identifier: artifact-identifier }))
)

;; Validates string content against specified length boundaries
(define-private (validate-string-boundaries (content (string-ascii 64)) (minimum-length uint) (maximum-length uint))
  (and 
    (>= (len content) minimum-length)
    (<= (len content) maximum-length)
  )
)

;; Examines individual classification tag for format compliance
(define-private (classification-tag-valid? (tag (string-ascii 32)))
  (and 
    (> (len tag) u0)
    (< (len tag) u33)
  )
)

;; Performs comprehensive validation on classification tag collection
(define-private (validate-classification-collection (tags (list 10 (string-ascii 32))))
  (and
    (> (len tags) u0)
    (<= (len tags) u10)
    (is-eq (len (filter classification-tag-valid? tags)) (len tags))
  )
)

;; Retrieves dimensional property from specified artifact record
(define-private (extract-artifact-dimension (artifact-identifier uint))
  (default-to u0 
    (get dimensional-value 
      (map-get? vault-artifact-storage { artifact-identifier: artifact-identifier })
    )
  )
)

;; Confirms creator authority for specified artifact
(define-private (verify-creator-authority? (artifact-identifier uint) (creator-principal principal))
  (match (map-get? vault-artifact-storage { artifact-identifier: artifact-identifier })
    artifact-data (is-eq (get creator-principal artifact-data) creator-principal)
    false
  )
)

;; Advances artifact counter and returns incremented value
(define-private (advance-artifact-counter)
  (let ((current-counter (var-get total-registered-artifacts)))
    (var-set total-registered-artifacts (+ current-counter u1))
    (ok current-counter)
  )
)

;; -----------------------------
;; PRIMARY DATA REPOSITORIES
;; -----------------------------

;; Central storage mechanism for digital artifact metadata
(define-map vault-artifact-storage
  { artifact-identifier: uint }
  {
    artifact-title: (string-ascii 64),
    creator-principal: principal,
    dimensional-value: uint,
    creation-block-height: uint,
    artifact-description: (string-ascii 128),
    classification-tags: (list 10 (string-ascii 32))
  }
)

;; Access control matrix for artifact viewing permissions
(define-map access-control-matrix
  { artifact-identifier: uint, requesting-principal: principal }
  { viewing-authorized: bool }
)

;; -----------------------------
;; PUBLIC PROTOCOL INTERFACES
;; -----------------------------

;; Establishes new artifact registration within the vault system
(define-public (establish-artifact-registration (title (string-ascii 64)) (dimension uint) (description (string-ascii 128)) (tags (list 10 (string-ascii 32))))
  (let
    (
      (next-artifact-id (+ (var-get total-registered-artifacts) u1))
    )
    ;; Comprehensive input validation protocol
    (asserts! (and (> (len title) u0) (< (len title) u65)) ERR-INVALID-IDENTIFIER-FORMAT)
    (asserts! (and (> dimension u0) (< dimension u1000000000)) ERR-DIMENSION-OUT-OF-BOUNDS)
    (asserts! (and (> (len description) u0) (< (len description) u129)) ERR-INVALID-IDENTIFIER-FORMAT)
    (asserts! (validate-classification-collection tags) ERR-INVALID-IDENTIFIER-FORMAT)

    ;; Commit artifact data to persistent storage
    (map-insert vault-artifact-storage
      { artifact-identifier: next-artifact-id }
      {
        artifact-title: title,
        creator-principal: tx-sender,
        dimensional-value: dimension,
        creation-block-height: block-height,
        artifact-description: description,
        classification-tags: tags
      }
    )

    ;; Initialize creator access permissions
    (map-insert access-control-matrix
      { artifact-identifier: next-artifact-id, requesting-principal: tx-sender }
      { viewing-authorized: true }
    )

    ;; Update global artifact counter
    (var-set total-registered-artifacts next-artifact-id)
    (ok next-artifact-id)
  )
)

;; Validates title string format according to protocol specifications
(define-public (validate-title-format (title (string-ascii 64)))
  (ok (and (> (len title) u0) (<= (len title) u64)))
)

;; Retrieves comprehensive artifact description from vault storage
(define-public (retrieve-artifact-description (artifact-identifier uint))
  (let
    (
      (artifact-data (unwrap! (map-get? vault-artifact-storage { artifact-identifier: artifact-identifier }) ERR-ARTIFACT-NOT-FOUND))
    )
    (ok (get artifact-description artifact-data))
  )
)

;; Executes ownership transfer protocol for specified artifact
(define-public (execute-ownership-transfer (artifact-identifier uint) (new-creator principal))
  (let
    (
      (artifact-data (unwrap! (map-get? vault-artifact-storage { artifact-identifier: artifact-identifier }) ERR-ARTIFACT-NOT-FOUND))
    )
    (asserts! (artifact-registered? artifact-identifier) ERR-ARTIFACT-NOT-FOUND)
    (asserts! (is-eq (get creator-principal artifact-data) tx-sender) ERR-UNAUTHORIZED-ACCESS)

    ;; Update ownership record in storage
    (map-set vault-artifact-storage
      { artifact-identifier: artifact-identifier }
      (merge artifact-data { creator-principal: new-creator })
    )
    (ok true)
  )
)

;; Executes comprehensive artifact metadata modification
(define-public (execute-artifact-modification (artifact-identifier uint) (updated-title (string-ascii 64)) (updated-dimension uint) (updated-description (string-ascii 128)) (updated-tags (list 10 (string-ascii 32))))
  (let
    (
      (artifact-data (unwrap! (map-get? vault-artifact-storage { artifact-identifier: artifact-identifier }) ERR-ARTIFACT-NOT-FOUND))
    )
    ;; Authority and validation verification protocol
    (asserts! (artifact-registered? artifact-identifier) ERR-ARTIFACT-NOT-FOUND)
    (asserts! (is-eq (get creator-principal artifact-data) tx-sender) ERR-UNAUTHORIZED-ACCESS)
    (asserts! (and (> (len updated-title) u0) (< (len updated-title) u65)) ERR-INVALID-IDENTIFIER-FORMAT)
    (asserts! (and (> updated-dimension u0) (< updated-dimension u1000000000)) ERR-DIMENSION-OUT-OF-BOUNDS)
    (asserts! (and (> (len updated-description) u0) (< (len updated-description) u129)) ERR-INVALID-IDENTIFIER-FORMAT)
    (asserts! (validate-classification-collection updated-tags) ERR-INVALID-IDENTIFIER-FORMAT)

    ;; Commit updated metadata to storage
    (map-set vault-artifact-storage
      { artifact-identifier: artifact-identifier }
      (merge artifact-data { 
        artifact-title: updated-title, 
        dimensional-value: updated-dimension, 
        artifact-description: updated-description, 
        classification-tags: updated-tags 
      })
    )
    (ok true)
  )
)

;; Verifies access authorization for specified principal and artifact
(define-public (verify-principal-access-rights (artifact-identifier uint) (requesting-principal principal))
  (let
    (
      (access-record (map-get? access-control-matrix { artifact-identifier: artifact-identifier, requesting-principal: requesting-principal }))
    )
    (ok (is-some access-record))
  )
)

;; Calculates total classification tags associated with artifact
(define-public (calculate-classification-count (artifact-identifier uint))
  (let
    (
      (artifact-data (unwrap! (map-get? vault-artifact-storage { artifact-identifier: artifact-identifier }) ERR-ARTIFACT-NOT-FOUND))
    )
    (ok (len (get classification-tags artifact-data)))
  )
)

;; Executes permanent artifact removal from vault system
(define-public (execute-artifact-removal (artifact-identifier uint))
  (let
    (
      (artifact-data (unwrap! (map-get? vault-artifact-storage { artifact-identifier: artifact-identifier }) ERR-ARTIFACT-NOT-FOUND))
    )
    (asserts! (artifact-registered? artifact-identifier) ERR-ARTIFACT-NOT-FOUND)
    (asserts! (is-eq (get creator-principal artifact-data) tx-sender) ERR-UNAUTHORIZED-ACCESS)

    ;; Remove artifact from vault storage
    (map-delete vault-artifact-storage { artifact-identifier: artifact-identifier })
    (ok true)
  )
)



