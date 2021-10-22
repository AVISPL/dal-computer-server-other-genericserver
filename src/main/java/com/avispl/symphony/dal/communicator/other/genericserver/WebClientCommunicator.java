/*
 * Copyright (c) 2015-2021 AVI-SPL, Inc. All Rights Reserved.
 */
package com.avispl.symphony.dal.communicator.other.genericserver;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.avispl.symphony.api.dal.dto.monitor.ExtendedStatistics;
import com.avispl.symphony.api.dal.dto.monitor.Statistics;
import com.avispl.symphony.api.dal.error.ResourceNotReachableException;
import com.avispl.symphony.api.dal.monitor.Monitorable;
import com.avispl.symphony.dal.communicator.HttpCommunicator;
import com.avispl.symphony.dal.communicator.other.genericserver.utils.HttpStatus;
import com.avispl.symphony.dal.util.StringUtils;

/**
 * This class checks accessible to a given URI then return the HTTP status code to Symphony
 */
public class WebClientCommunicator extends HttpCommunicator implements Monitorable {

	private static final String WHITE_SPACE = " ";
	private static final String URI_STATUS = "URI Status";
	private static final String NOT_CONFIGURED = "Not Configured";
	/**
	 * URI string that is used to check accessible.
	 */
	private String URI;
	private HttpClientConnector httpClientConnector = new HttpClientConnector();

	/**
	 * Constructor
	 */
	public WebClientCommunicator() {
		// WebClientCommunicator no-args constructor
	}

	/**
	 * Retrieves {@code URI}
	 *
	 * @return String This returns the current URI
	 */
	public String getURI() {
		return this.URI;
	}

	/**
	 * Sets {@code URI}
	 *
	 * @param URI This is the URI to be set
	 */
	public void setURI(String URI) {
		this.URI = URI;
	}

	/**
	 * This method is called by Symphony to get the list of statistics to be displayed
	 * {@inheritDoc}
	 *
	 * @return List<Statistics> This returns the list of statistics
	 */
	@Override
	public List<Statistics> getMultipleStatistics() throws Exception {
		if (logger.isDebugEnabled()) {
			logger.debug(String.format("Perform doGet() at host %s with port %s", this.host, this.getPort()));
		}
		final ExtendedStatistics extStats = new ExtendedStatistics();
		final Map<String, String> stats = new HashMap<>();
		String uriStatusMessage;
		if (!StringUtils.isNullOrEmpty(this.URI)) {
			int statusCode = httpClientConnector.doGet(this.URI);
			if (!HttpStatus.containsKey(statusCode)) {
				throw new ResourceNotReachableException("Response status code not in range");
			}
			uriStatusMessage = generateResponseMessage(statusCode);
			stats.put(URI_STATUS, uriStatusMessage);
		} else {
			stats.put(URI_STATUS, NOT_CONFIGURED);
		}
		extStats.setStatistics(stats);
		return Collections.singletonList(extStats);
	}

	@Override
	protected void internalInit() throws Exception {
		httpClientConnector.setHost(this.getHost());
		httpClientConnector.setProtocol(this.getProtocol());
		httpClientConnector.setPort(this.getPort());
		httpClientConnector.setBaseUri(this.getBaseUri());
		httpClientConnector.setTrustAllCertificates(this.getTrustAllCertificates());
		httpClientConnector.setMaxConnectionsPerRoute(this.getMaxConnectionsPerRoute());
		httpClientConnector.setMaxConnectionsTotal(this.getMaxConnectionsTotal());
		httpClientConnector.setTimeout(this.getTimeout());
		super.internalInit();
	}

	/**
	 * WebClientCommunicator doesn't require authentication
	 * {@inheritDoc}
	 */
	@Override
	protected void authenticate() {
		// WebClientCommunicator doesn't require authentication
	}

	/**
	 * Return a string with status code & its description
	 *
	 * @return String This returns the message as "<status code> <description>"
	 */
	private String generateResponseMessage(int statusCode) {
		return statusCode + WHITE_SPACE + HttpStatus.getDescription(statusCode);
	}
}