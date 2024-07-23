package com.dormitory.controller.web.admin;


import io.swagger.annotations.Api;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * <p>
 * 调换申请表 前端控制器
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@RestController
@RequiredArgsConstructor
@RequestMapping("/admin/change/apply")
@Api(value = "AdminChangeApplyController", tags = {"调换申请接口"})
public class AdminChangeApplyController {

}

