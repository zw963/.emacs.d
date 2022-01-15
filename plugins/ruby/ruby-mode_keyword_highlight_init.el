(defcustom ruby-general-keywords
  '("__LINE__" "__ENCODING__" "__FILE__" "at_exit" "attr" "attr_accessor"
    "attr_reader" "load" "attr_writer" "loop" "proc" "lambda" "eval" "exec"
    "exit" "exit!" "system" "fork" "included" "class_methods" "private_class_method"
    )
  "List of keywords to highlight for spec."
  :group 'rinari
  :type '(repeat string)
  )

(defcustom ruby-additional-keywords
  '("require" "load" "require_relative" "extend" "include" "prepend" "abort"
    "fail" "warn" "block_given?" "alias_method" "catch" "module_function"
    "public" "private" "protected" "raise" "throw" "pry!" "pry1" "pry2" "pry3"
    "repry!" "pry?" "def_delegators" "def_delegator" "irb1" "irb2" "irb!" "on"
    )
  "List of keywords to highlight for spec."
  :group 'rinari
  :type '(repeat string)
  )

;; stolen from rinari
(defun ruby-highlight-keywords (keywords &optional face)
  "Highlight the passed KEYWORDS FACE in current buffer.
Use `font-lock-add-keywords' in case of `ruby-mode' or
`ruby-extra-keywords' in case of Enhanced Ruby Mode."
  (if (boundp 'enh-ruby-extra-keywords)
      (progn
        (setq enh-ruby-extra-keywords (append enh-ruby-extra-keywords keywords))
        (enh-ruby-local-enable-extra-keywords))
    (font-lock-add-keywords
     nil
     (list (list
            (concat "\\(^\\|[^_:.@$]\\|\\.\\.\\)\\b"
                    (regexp-opt keywords t)
                    (eval-when-compile (if (string-match "\\_>" "ruby")
                                           "\\_>"
                                         "\\>")))
            (list 2 (or face 'font-lock-builtin-face)))))))

(defcustom rinari-controller-keywords
  '("logger" "polymorphic_path" "polymorphic_url" "mail" "render" "attachments"
    "default" "helper" "helper_attr" "helper_method" "layout" "url_for"
    "serialize" "exempt_from_layout" "filter_parameter_logging" "hide_action"
    "cache_sweeper" "protect_from_forgery" "caches_page" "cache_page"
    "caches_action" "expire_page" "expire_action" "rescue_from" "params"
    "request" "response" "session" "flash" "head" "redirect_to"
    "render_to_string" "respond_with"
    ;; Rails < 4
    "before_filter" "append_before_filter"
    "prepend_before_filter" "after_filter" "append_after_filter"
    "prepend_after_filter" "around_filter" "append_around_filter"
    "prepend_around_filter" "skip_before_filter" "skip_after_filter" "skip_filter"
    ;; Rails >= 4
    "after_action" "append_after_action" "append_around_action"
    "append_before_action" "around_action" "before_action" "prepend_after_action"
    "prepend_around_action" "prepend_before_action" "skip_action_callback"
    "skip_after_action" "skip_around_action" "skip_before_action" "add_flash_types")
  "List of keywords to highlight for controllers."
  :group 'rinari
  :type '(repeat string))

(defcustom rinari-model-keywords
  '("default_scope" "named_scope" "scope" "serialize" "belongs_to" "has_one"
    "has_many" "has_and_belongs_to_many" "composed_of" "accepts_nested_attributes_for"
    "before_create" "before_destroy" "before_save" "before_update" "before_validation"
    "before_validation_on_create" "before_validation_on_update" "after_create"
    "after_destroy" "after_save" "after_update" "after_validation"
    "after_validation_on_create" "after_validation_on_update" "around_create"
    "around_destroy" "around_save" "around_update" "after_commit" "after_find"
    "after_initialize" "after_rollback" "after_touch" "attr_accessible"
    "attr_protected" "attr_readonly" "validates" "validate" "validate_on_create"
    "validate_on_update" "validates_acceptance_of" "validates_associated"
    "validates_confirmation_of" "validates_each" "validates_exclusion_of"
    "validates_format_of" "validates_inclusion_of" "validates_length_of"
    "validates_numericality_of" "validates_presence_of" "validates_size_of"
    "validates_uniqueness_of" "validates_with" "store" "store_accessor"
    "enum" "delegate" "has_secure_password" "has_secure_token" "after_create_commit"
    "after_update_commit" "thread_mattr_accessor" "class_attribute")
  "List of keywords to highlight for models."
  :group 'rinari
  :type '(repeat string))

(defcustom rinari-migration-keywords
  '("create_table" "change_table" "drop_table" "rename_table" "add_column"
    "rename_column" "change_column" "change_column_default" "remove_column"
    "add_index" "remove_index" "rename_index" "execute" "add_reference"
    "change_column_null" "remove_reference" "change_column_comment"
    "change_table_comment" "create_join_table")
  "List of keywords to highlight for migrations."
  :group 'rinari
  :type '(repeat string))

(defcustom ruby-rake-keywords
  '("task" "namespace" "file" "rule" "directory" "desc")
  "List of keywords to highlight for rake task."
  :group 'rinari
  :type '(repeat string))

(defcustom ruby-gemfile-keywords
  '("gem" "source" "group" "git")
  "List of keywords to highlight for spec."
  :group 'rinari
  :type '(repeat string)
  )

(defcustom ruby-spec-keywords
  '("before" "after" "describe" "it" "test" "specify" "subject" "context" "expect" "allow" "let" "let!"
    "receive" "receive_messages" "receive_message_chain" "and_return" "and_raise" "and_throw" "and_yield"
    "and_call_original" "and_wrap_original" "allow_any_instance_of" "expect_any_instance_of" "satisfy"
    "to" "not_to" "be" "be_empty" "be_between" "inclusive" "exclusive" "be_truthly" "be_falsey" "be_nil"
    "be_instance_of" "be_kind_of" "be_within" "start_with" "end_with" "respond_to" "raise_error" "throw_symbol"
    "have_key" "include" "match_array" "have_attributes" "match" "cover" "change" "from" "to" "by" "by_at_least" "by_at_most"
    "output" "to_stdout" "to_stderr" "yield_control" "yield_with_no_args" "yield_with_args" "yield_successive_args"
    "exactly" "once" "twice" "times" "at_least" "spy" "is_expected" "should" "assigns" "assert_redirect_to"
    "setup" "skip" "have_and_belong_to_many" "belong_to" "have_many" "validate_presence_of"
    "validate_inclusion_of" "have_attached_file" "validate_exclusion_of" "validate_numericality_of"
    "validate_uniqueness_of" "validate_confirmation_of" "allow_mass_assignment_of" "have_secure_password"
    "allow_value" "validates_format_of" "ensure_length_of" "teardown" "have_one"
    "allow_values" "validate_length_of" "should_not" "perform_enqueued_jobs" "delegate_method"
    "define_enum_for"
    "assert" "assert_equal" "assert_response" "assert_same_elements" "assert_includes" "assert_empty"
    "assert_instance_of" "assert_kind_of" "assert_match" "assert_nil" "assert_output" "assert_raises"
    "assert_response_to" "assert_same" "assert_silent" "assert_throws" "assert_enqueued_with"
    "assert_mock" "assert_in_delta"
    "refute" "refute_nil" "refute_empty" "refute_empty" "refute_same" "refute_in_delta" "refute_respond_to"
    "refute_in_epsilon" "refute_includes" "refute_instance_of" "refute_kind_of" "refute_match"
    )
  "List of keywords to highlight for spec."
  :group 'rinari
  :type '(repeat string)
  )

(defcustom rails-route-keywords
  '("member" "collection" "resources" "resource" "scope" "concern" "concerns" "namespace")
  "List of keywords to highlight for rails route."
  :group 'rinari
  :type '(repeat string)
  )

(defcustom ruby-factory-girl-keywords
  '("factory" "trait" "sequence" "after" "create" "create_list" "transient")
  "List of keywords to highlight for factory girl."
  :group 'rinari
  :type '(repeat string)
  )

;; 设定那些关键字之前, 运行 ruby-end-of-block
(defconst ruby-block-start-keywords
  '( "loop" "before" "after" "describe" "it" "test" "specify" "subject"
     "member" "collection" "resources" "resource" "scope" "task" "namespace"
     "context" "factory" "should"
     "each"
     )
  "Keywords that start a block.")


;; stolen from rinari
(defun ruby-apply-keywords-for-file-type ()
  "Apply extra font lock keywords specific to models, controllers etc."
  (when (buffer-file-name)
    (cl-loop for (re keywords) in
          `(("_controller\\.rb$"   ,rinari-controller-keywords)
            ("app/models/.+\\.rb$" ,rinari-model-keywords)
            ("db/migrate/.+\\.rb$" ,rinari-migration-keywords)
            (".+\\.rake$\\|Rakefile$" ,ruby-rake-keywords)
            ("Gemfile" ,ruby-gemfile-keywords)
            (".+_spec\\.rb$\\|.+_test\.rb$" ,ruby-spec-keywords)
            ("config/routes.*\\.rb$" ,rails-route-keywords)
            ("factories.*\\.rb$" ,ruby-factory-girl-keywords)
            )
          do (when (string-match-p re (buffer-file-name))
               (ruby-highlight-keywords keywords 'font-lock-builtin-face)
               ))))

(defun ruby-apply-keywords-locally ()
  (ruby-highlight-keywords ruby-general-keywords 'font-lock-builtin-face) ; 白色
  (ruby-highlight-keywords ruby-additional-keywords 'font-lock-keyword-face) ; 暗金色
  (ruby-apply-keywords-for-file-type)
  )

(run-ruby-mode-hook '(ruby-apply-keywords-locally))

(provide 'ruby-mode_keyword_highlight_init)

;;; ruby-mode_keyword_highlight_init.el ends here
