package com.dormitory.controller.web.student;


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
@RequestMapping("/student/change/apply")
@Api(value = "StudentChangeApplyController", tags = {"调换申请接口"})
public class StudentChangeApplyController {
    
}

