package com.github.derg.transpiler.experimental.resolver

/**
 * The failure mode which occurred during the processing. Certain symbols cannot be processed for any reason. All
 * possible failure modes are encapsulated by the outcome.
 */
sealed interface Outcome
{
    /**
     * A temporary data structure used to simply gather information. We use this placeholder as a way to pass error
     * cases out from the workers, before we know what the error cases actually are.
     */
    data class Placeholder(val message: String) : Outcome
}
